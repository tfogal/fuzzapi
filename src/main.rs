extern crate rand;
extern crate tempdir;
use std::fs::File;
use std::path::Path;
use std::process::Command;
use tempdir::TempDir;
mod api;
mod function;
mod fuzz;
mod generator;
mod stmt;
mod tc;
mod typ;
mod usergen;
mod util;
mod variable;
use function::*;
use typ::*;
use usergen::UserGen;

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

// This is mostly used for testing.
#[allow(dead_code)]
fn state(strm: &mut std::io::Write, fqns: &Vec<&Function>) {
	for fqn in fqns {
		tryp!(write!(strm, "{}(", fqn.name));
		for (a, arg) in fqn.arguments.iter().enumerate() {
			match arg.expr {
				stmt::Expression::Basic(_, ref sym) => {
					tryp!(write!(strm, "{:?}", sym.generator));
				},
				_ => (),
			};
			if a != fqn.arguments.len()-1 {
				tryp!(write!(strm, ", "));
			}
		}
		tryp!(writeln!(strm, ")"));
	}
}

fn system(cmd: &str) -> Result<(), String> {
	let run = match Command::new(cmd.clone()).output() {
		Err(e) => {
			use std::fmt;
			// cat 'run' ...
			return Err(fmt::format(format_args!("exec {} error: {}", cmd, e)));
		},
		Ok(x) => x,
	};
	let err = String::from_utf8(run.stderr).unwrap();
	if err.len() > 0 || !run.status.success() {
		use std::fmt;
    return Err(fmt::format(format_args!("execution of {} failed: {}",
		                                    cmd, err)));
	}

	let runout: String = String::from_utf8(run.stdout).unwrap();
	if runout.len() > 0 {
		println!("program output: '{}'", runout);
	}
	return Ok(());
}

fn compile(src: &str, dest: &str, flags: &Vec<&str>) -> Result<(), String> {
	let mut cmd = Command::new("gcc");
	for flg in flags.iter() {
		cmd.arg(flg);
	}
	cmd.arg("-o");
	cmd.arg(dest);
	cmd.arg(src);

	let compile = match cmd.output() {
		Err(e) => {
			use std::fmt;
			let s = fmt::format(format_args!("{} compilation failed: {}", src, e));
			return Err(s);
		},
		Ok(x) => x,
	};

	let err = String::from_utf8(compile.stderr).unwrap();
	if err.len() > 0 && !compile.status.success() {
		use std::fmt;
		return Err(fmt::format(format_args!("compilation of {} failed: {}",
		                                    src, err)));
	}

	let comps: String = String::from_utf8(compile.stdout).unwrap();
	if comps.len() > 0 {
		println!("gcc output: '{}'", comps);
		return Err("compilation failed".to_string());
	}
	match cmd.status() {
		Err(e) => return Err(format!("err: {}", e)),
		Ok(_) => return Ok(()),
	}
}

fn compile_and_test_program(program: &api::Program) -> Result<(),String> {
	use std::fmt;

	let tmpdir = match TempDir::new("hotfuzz") {
		Err(e) => return Err(fmt::format(format_args!("tmp dir: {}", e))),
		Ok(t) => t,
	};
	let fname = tmpdir.path().join("fuzziter.c");
	let mut newtest = match File::create(fname.clone()) {
		Err(e) => {
			println!("Could not create {:?}: {}", fname, e); /* FIXME stderr */
			return Err(fmt::format(format_args!("creating {:?}: {}", fname, e)));
		},
		Ok(x) => x,
	};

	let hdrs: Vec<&str> = vec!["stdlib.h", "search.h"];
	match program.prologue(&mut newtest, &hdrs) {
		Err(x) => return Err(fmt::format(format_args!("prologue: {}", x))),
		_ => (),
	};
	match program.codegen(&mut newtest) {
		Err(x) => return Err(fmt::format(format_args!("codegen: {}", x))),
		_ => (),
	}
	match program.epilogue(&mut newtest) {
		Err(x) => return Err(fmt::format(format_args!("epilogue: {}", x))),
		_ => (),
	}
	drop(newtest);

	let outname = tmpdir.path().join(".fuzziter");
	let outnm: &str = outname.to_str().unwrap();
	let args = vec!["-Wall", "-Wextra", "-fcheck-pointer-bounds", "-mmpx",
									"-D_GNU_SOURCE", "-UNDEBUG"];
	match compile(fname.to_str().unwrap(), outnm, &args) {
		Err(x) => return Err(x),
		Ok(_) => {},
	};

	let out = system(outnm);
	if out.is_err() {
		use std::io::Write;
		writeln!(&mut std::io::stderr(), "Execution error: {}",
		         out.err().unwrap()).unwrap();
		std::process::exit(1);
	}
	return Ok(());
}

fn tobox(orig: Vec<UserGen>) -> Vec<Box<variable::Generator>> {
	let mut rv: Vec<Box<variable::Generator>> = Vec::new();
	for v in orig.iter() {
		rv.push(Box::new((*v).clone()));
	}
	return rv;
}

// instantiate generators for builtin, non-parameterized types.
fn builtin_generators() -> Vec<Box<variable::Generator>> {
	let mut rv: Vec<Box<variable::Generator>> = Vec::new();

	rv.push(Box::new(variable::GenNothing{}));
	rv.push(Box::new(variable::GenI32::create(&Type::Builtin(Native::I32))));
	rv.push(Box::new(variable::GenUsize::create(&Type::Builtin(Native::Usize))));
	rv.push(variable::natgenerator(&Native::Integer));

	return rv;
}

// instantiate generators specific to the hash test case infrastructure
fn hash_generators() -> Vec<Box<variable::Generator>> {
	let mut rv: Vec<Box<variable::Generator>> = Vec::new();

	let hs_data = Type::Struct("struct hsearch_data".to_string(), vec![]);
	let hs_data_ptr: Type = Type::Pointer(Box::new(hs_data.clone()));
	rv.push(Box::new(variable::GenOpaque::create(&hs_data_ptr)));

	let char_ptr = Type::Pointer(Box::new(Type::Builtin(Native::Character)));
	let void_ptr = Type::Pointer(Box::new(Type::Builtin(Native::Void)));
	let entry = Type::Struct("ENTRY".to_string(),
		vec![("key".to_string(), Box::new(char_ptr)),
		     ("data".to_string(), Box::new(void_ptr))]
	);
	rv.push(variable::generator(&entry));
	rv.push(variable::generator(&Type::Pointer(Box::new(entry))));

	let action_values = vec![("FIND".to_string(), 0),
	                         ("ENTER".to_string(), 1)];
	let action = Type::Enum("ACTION".to_string(), action_values);
	rv.push(variable::generator(&action));
	return rv;
}

// parses user generators from the given file.
fn parse_generators(fname: &str) -> Vec<Box<variable::Generator>> {
	let p = Path::new(fname);
	let mut fp = match File::open(&p) {
		Err(e) => panic!("error reading {}: {}", fname, e),
		Ok(f) => f,
	};
	let mut s = String::new();
	use std::io::Read;
	fp.read_to_string(&mut s).unwrap();
	let lgen = generator::parse_LGeneratorList(s.as_str());
	let stdgen: Vec<UserGen> = match lgen {
		Err(e) => panic!("err reading {:?}: {:?}", p, e),
		Ok(a) => a,
	};
	tobox(stdgen)
}

fn main() {
	// todo: search path for hf files.
	let mut generators = parse_generators("../share/stdgen.hf");
	generators.append(&mut builtin_generators());
	generators.append(&mut hash_generators());
	let generators: Vec<Box<variable::Generator>> = generators; // drop mut.

	let s = "struct hsearch_data {}\n".to_string() +
		"struct entry { pointer char key; pointer void data; }\n" +
		"enum ACTION { FIND = 0 , ENTER = 1 , }" +
		"var:free nel gen:Usize usize\n" +
		"var:free tbl gen:opaque struct hsearch_data\n" +
		/* no current way to represent hcreate_r return value .. */
		"var:free item gen:udt struct entry\n" +
		"var:free actvar gen:Enum enum ACTION\n" +
		// note the API has a fqn argument named "retval"!
		"var:free retval gen:udt pointer struct entry\n" +
		/* no current way to represent hsearch_r's retval ... */
		"function:new hcreate_r int {\n" +
			"usize, pointer struct hsearch_data,\n" +
		"}\n" +
		"function:new hsearch_r int {\n" +
			"int, int, pointer pointer int, pointer struct hsearch_data,\n" +
		"}\n" +
		"function:call hcreate_r { nel op:& tbl }\n" +
		"function:call hsearch_r { item actvar op:& retval op:& tbl }\n";
	let mut lprogram = match fuzz::parse_LProgram(s.as_str()) {
		Err(e) => panic!("{:?}", e),
		Ok(x) => x,
	};
	assert!(lprogram.declarations.len() > 1);
	lprogram.set_generators(&generators);
	match lprogram.analyze() {
		Err(e) => panic!(e),
		_ => (),
	};
	assert_eq!(lprogram.statements.len(), 7);

	while !lprogram.done() {
		match compile_and_test_program(&lprogram) {
			Err(e) => {
				let mut failed: Vec<u8> = Vec::new();
				lprogram.codegen(&mut failed).unwrap();
				let prog = String::from_utf8(failed).unwrap();
				panic!("Program {} failed: {}.", prog, e);
			},
			Ok(_) => (),
		};
		lprogram.next();
	}
	// We next()ed, but then our iteration finished() before we actually
	// compile_and_test()ed the final state.  Test that last state.
	match compile_and_test_program(&lprogram) {
		Err(e) => panic!("compile/test error: {}", e),
		Ok(_) => {},
	};
}

#[cfg(test)]
mod test {
	use super::*;
	use std::fs::File;

	fn generators_for_test() -> Vec<Box<variable::Generator>> {
		let mut gen = parse_generators("./share/stdgen.hf"); // todo search path
		gen.append(&mut builtin_generators());
		gen.append(&mut hash_generators());
		gen
	}

	// dumps WHAT into the file named TO.  panics on any errors.
	#[allow(dead_code)]
	fn dump(to: &str, what: &str) {
		let p = Path::new(to);
		let mut fp: File = match File::create(to) {
			Err(e) => panic!("could not open {}: {:?}", to, e),
			Ok(x) => x,
		};
		use std::io::Write;
		match fp.write(what.as_bytes()) {
			Err(e) => {
				match std::fs::remove_file(p) { _ => () };
				panic!("write of {} failed: {}", to, e);
			},
			Ok(_) => (),
		};
		drop(fp);
	}

	fn hs_program() -> String {
		let s = "struct hsearch_data {}\n".to_string() +
			"struct entry { pointer char key; pointer void data; }\n" +
			"enum ACTION { FIND = 0 , ENTER = 1 , }" +
			"var:free nel gen:Usize usize\n" +
			"var:free tbl gen:opaque struct hsearch_data\n" +
			/* no current way to represent hcreate_r return value .. */
			"var:free item gen:udt struct entry\n" +
			"var:free actvar gen:Enum enum ACTION\n" +
			// note the API has a fqn argument named "retval"!
			"var:free retval gen:udt pointer struct entry\n" +
			/* no current way to represent hsearch_r's retval ... */
			"function:new hcreate_r int {\n" +
				"usize, pointer struct hsearch_data,\n" +
			"}\n" +
			"function:new hsearch_r int {\n" +
				"int, int, pointer pointer int, pointer struct hsearch_data,\n" +
			"}\n" +
			"function:call hcreate_r { nel op:& tbl }\n" +
			"function:call hsearch_r { item actvar op:& retval op:& tbl }\n";
		s
	}

	#[test]
	fn parse_stdgen() {
		let mut generators = parse_generators("./share/stdgen.hf"); // todo search
		assert!(generators.len() > 0);
		generators.append(&mut builtin_generators());
		generators.append(&mut hash_generators());
	}

	#[test]
	// ensures the iteration terminates.
	fn test_iter() {
		let s = "struct hsearch_data {}\n".to_string() +
			"var:free nel gen:Usize usize\n" +
			"var:free tbl gen:opaque struct hsearch_data\n" +
			"function:new hcreate_r int {\n" +
				"usize, pointer struct hsearch_data,\n" +
			"}\n" +
			"function:call hcreate_r { nel op:& tbl }\n";
		let mut lprogram = match fuzz::parse_LProgram(s.as_str()) {
			Err(e) => panic!("{:?}", e),
			Ok(x) => x,
		};
		match lprogram.analyze() {
			Err(e) => panic!(e),
			_ => (),
		};
		println!("program: {:?}", lprogram);
		assert_eq!(lprogram.statements.len(), 3);
		lprogram.set_generators(&generators_for_test());

		while !lprogram.done() {
			lprogram.next();
		}
	}

	#[test]
	fn nonzero_num_iter() {
		let s = hs_program();
		let mut lprogram = match fuzz::parse_LProgram(s.as_str()) {
			Err(e) => panic!("{:?}", e),
			Ok(x) => x,
		};
		assert!(lprogram.declarations.len() > 1);
		lprogram.set_generators(&generators_for_test());
		match lprogram.analyze() {
			Err(e) => panic!(e),
			_ => (),
		};
		assert_eq!(lprogram.statements.len(), 7);
		assert_eq!(lprogram.n_states(), 256);
	}

	#[test]
	fn parse_hash_decls() {
		let s = "struct hsearch_data {}\n".to_string() +
			"var:free tbl gen:opaque struct hsearch_data\n" +
			"function:new hcreate_r int {\n" +
				"usize, pointer struct hsearch_data,\n" +
			"}\n" +
			"function:new hsearch_r int {\n" +
				"int, int, pointer pointer int, pointer struct hsearch_data,\n" +
			"}";
		match fuzz::parse_LProgram(s.as_str()) {
			Err(e) => panic!("{:?}", e),
			Ok(x) => x,
		};
	}

	#[test]
	fn hash_table_search_case() {
		let s = hs_program();
		let mut lprogram = match fuzz::parse_LProgram(s.as_str()) {
			Err(e) => panic!("{:?}", e),
			Ok(x) => x,
		};
		assert!(lprogram.declarations.len() > 1);
		match lprogram.analyze() {
			Err(e) => panic!(e),
			_ => (),
		};
		assert_eq!(lprogram.statements.len(), 7);
		let hdrs: Vec<&str> = vec!["stdlib.h", "search.h"];
		let mut strm: Vec<u8> = Vec::new();
		match lprogram.prologue(&mut strm, &hdrs) {
			Err(e) => panic!(e), Ok(_) => (),
		};
		match lprogram.codegen(&mut strm) {
			Err(e) => panic!(e), Ok(_) => (),
		}
		match lprogram.epilogue(&mut strm) {
			Err(e) => panic!(e), Ok(_) => (),
		}
		drop(strm);
	}
}
