extern crate rand;
extern crate tempdir;
use std::collections::HashSet;
use std::fs::File;
use std::mem;
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
use stmt::*;
use typ::*;
use usergen::UserGen;

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

fn header(strm: &mut std::io::Write, hdrs: &Vec<&str>) -> std::io::Result<()>
{
	try!(writeln!(strm, "#define _POSIX_C_SOURCE 201212L"));
	try!(writeln!(strm, "#define _GNU_SOURCE 1"));
	for h in hdrs.iter() {
		try!(writeln!(strm, "#include <{}>", h));
	}
	return Ok(());
}

fn reset(fqn: &mut Function) {
	for i in 0..fqn.arguments.len() {
		if fqn.arguments[i].source().is_free() {
			//arg.src.borrow_mut().generator.deref_mut().reset();
			fqn.arguments[i].reset();
		}
	}
}

// A function is finished its iteration if all free arguments' generators are
// finished iterating.  But we specially construct non-free arguments'
// generators such that they always say they are done, so we can just ask if
// all arguments are done.
fn fqnfinished(func: &Function) -> bool {
	return func.arguments.iter().all(
		|ref arg| arg.done()
	);
}

// The whole computation is finished if all functions are finished.
fn finished(functions: &Vec<&Function>) -> bool {
	return functions.iter().all(|ref f| fqnfinished(f));
}

// Advance to the next state for the given function.
// Undefined behavior if the function is finished.
fn fqnnext(func: &mut Function) {
	assert!(!fqnfinished(func));
	// Find the "rightmost" (last) argument that is incomplete.
	let nxt = match func.arguments.iter().rposition(|ref a| {
		!a.done()
	}) {
		None => { // this implies the function is finished.  Bug somewhere.
			panic!("Bug: function {} not finished, but can't find non-done arg?",
			       func.name);
		},
		Some(idx) => idx,
	};
	assert!(!func.arguments[nxt].done());
	// Iterate that "rightmost unfinished" argument.
	func.arguments[nxt].next();

	// reset all subsequent arguments.
	for idx in nxt+1..func.arguments.len() {
		func.arguments[idx].reset()
	}
}

fn next(functions: &mut Vec<&mut Function>) {
	{
		let immut = unsafe {
			mem::transmute::<&Vec<&mut Function>, &Vec<&Function>>(functions)
		};
		assert!(!finished(&immut));
	}

	// find the right-most function that is not finished ...
	let nxt = match functions.iter().rposition(|ref f| !fqnfinished(&f)) {
		None => unreachable!(),
		Some(idx) => idx,
	};
	fqnnext(&mut functions[nxt]); // ... advance that function ...
	// ... and reset all the functions after that.
	for idx in nxt+1..functions.len() {
		reset(&mut functions[idx]);
	}
}

// This is mostly used for testing.
#[allow(dead_code)]
fn state(strm: &mut std::io::Write, fqns: &Vec<&Function>) {
	for fqn in fqns {
		tryp!(write!(strm, "{}(", fqn.name));
		for (a, arg) in fqn.arguments.iter().enumerate() {
			if arg.source().is_free() {
				tryp!(write!(strm, "{:?}", arg.source().generator));
			} else if arg.source().is_bound() {
				tryp!(write!(strm, "(bound)"));
			} else if arg.source().is_retval() {
				tryp!(write!(strm, "(rv)"));
			}
			if a != fqn.arguments.len()-1 {
				tryp!(write!(strm, ", "));
			}
		}
		tryp!(writeln!(strm, ")"));
	}
}

fn gen(strm: &mut std::io::Write, fqns: &Vec<&Function>) -> std::io::Result<()>
{
	let hdrs: Vec<&str> = vec!["stdlib.h", "search.h"];
	try!(header(strm, &hdrs));
	try!(writeln!(strm, "")); // just a newline to separate them out.

	let pgm = api::Program::new(&vec![], &vec![]);

	// todo: we need something more intelligent than just a list of functions,
	// something more like an AST.  For now we just decree that the functions are
	// given in the order they should be called...
	try!(writeln!(strm, "int main() {{"));

	let mut seen: HashSet<String> = HashSet::new(); // don't declare vars twice.
	for fqn in fqns {
		// declare all variables used as arguments to the function
		for ref arg in fqn.arguments.iter() {
			if arg.source().is_free() && !seen.contains(&arg.decl()) {
				try!(writeln!(strm, "\t{}", arg.decl()));
				seen.insert(arg.decl());
			} // no else: all vars eventually come from a free
		};
		let ref ret = fqn.retval;
		// FIXME: declaration for the return value is commented out.  This is
		// because our current API doesn't have any bound variables for them, so
		// they just create unused variable warnings.
		// Another thing to consider is properties (postconditions) to verify.
		// When we add those it would be useful to be able to reference return
		// values.  Still, we might need to walk the variable graph and consider
		// only inserting the declaration when the variable is used.
		try!(write!(strm, "\t/*{} {} =*/ {}(", ret.ty.name(), ret.src.borrow().name(), fqn.name));

		for (a, ref arg) in fqn.arguments.iter().enumerate() {
			//let &(_, ref src) = *arg;
			try!(arg.codegen(strm, &pgm));
			if a < fqn.arguments.len()-1 {
				try!(write!(strm, ", "));
			}
		}
		try!(writeln!(strm, ");"));
	}

	try!(writeln!(strm, "\n\treturn 0;"));
	try!(writeln!(strm, "}}"));
	return Ok(());
}

fn system(cmd: String) -> Result<(), String> {
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

fn compile_and_test(api: &Vec<&mut Function>) -> Result<(),String> {
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

	let immut = unsafe {
		// &Vec<&mut T> doesn't coerce to &Vec<&T>.  Really.
		mem::transmute::<&Vec<&mut Function>,&Vec<&Function>>(&api)
	};
	match gen(&mut newtest, immut) {
		Err(x) => return Err(fmt::format(format_args!("gen: {}", x))),
		Ok(_) => {},
	};
	drop(newtest);

	let outname = tmpdir.path().join(".fuzziter");
	let args = vec!["-Wall", "-Wextra", "-fcheck-pointer-bounds", "-mmpx",
									"-D_GNU_SOURCE", "-UNDEBUG"];
	let outnm: &str = outname.to_str().unwrap();
	match compile(fname.to_str().unwrap(), outnm, &args) {
		Err(x) => return Err(x),
		Ok(_) => {},
	};

	let cmdname: String = String::from(outnm);
	let out = system(cmdname);
	if out.is_err() {
		use std::io::Write;
		writeln!(&mut std::io::stderr(), "Execution error: {}",
		         out.err().unwrap()).unwrap();
		std::process::exit(1);
	}
	return Ok(());
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

	let outname: &'static str = ".fuzziter";
	let args = vec!["-Wall", "-Wextra", "-fcheck-pointer-bounds", "-mmpx",
									"-D_GNU_SOURCE", "-UNDEBUG"];
	match compile(fname.to_str().unwrap(), outname, &args) {
		Err(x) => return Err(x),
		Ok(_) => {},
	};

	let cmdname: String = String::from("./") + String::from(outname).as_str();
	let out = system(cmdname);
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

fn parse_types(fname: &str, mut generators: &mut Vec<Box<variable::Generator>>)
	-> Vec<Type> {
	let p = Path::new(fname);
	let mut fp = match File::open(&p) {
		Err(e) => panic!("error reading {}: {}", fname, e),
		Ok(f) => f,
	};
	let mut s = String::new();
	use std::io::Read;
	fp.read_to_string(&mut s).unwrap();
	let decls: Vec<api::Declaration> = match fuzz::parse_LDeclarations(&s) {
		Err(e) => panic!("error parsing {}: {:?}", fname, e),
		Ok(x) => x,
	};

	let (types, _) = api::resolve_types(&decls, &mut generators);
	return types;
}

fn main() {
	let hs_data = Type::Struct("struct hsearch_data".to_string(), vec![]);
	let hs_data_ptr: Type = Type::Pointer(Box::new(hs_data.clone()));

	// todo: search path for hf files.
	let mut generators = parse_generators("../share/stdgen.hf");
	generators.append(&mut builtin_generators());
	generators.append(&mut hash_generators());
	let generators: Vec<Box<variable::Generator>> = generators; // drop mut.

	let char_ptr = Type::Pointer(Box::new(Type::Builtin(Native::Character)));
	let void_ptr = Type::Pointer(Box::new(Type::Builtin(Native::Void)));
	let entry = Type::Struct("ENTRY".to_string(),
		vec![("key".to_string(), Box::new(char_ptr)),
		     ("data".to_string(), Box::new(void_ptr))]
	);
	let action_values = vec![("FIND".to_string(), 0), ("ENTER".to_string(), 1)];
	let action = Type::Enum("ACTION".to_string(), action_values);

	use variable::ScalarOp;
	let nel = variable::Source::free("nel", &Type::Builtin(Native::Usize),
	                                 "std:usize", &generators);
	let genname = "std:opaque:struct hsearch_data*";
	let hsd_var = variable::Source::free("tbl", &hs_data, genname, &generators);
	let hsd_addr = Expression::Simple(ScalarOp::AddressOf,
	                                  hsd_var.borrow().clone());
	let fa1 = Argument::new(&Type::Builtin(Native::Usize), nel);
	let fa2 = Argument::newexpr(&hs_data, &hsd_addr);
	let hcreate_args = vec![fa1, fa2];

	let hc_retval = variable::Source::retval("crterr", "hcreate_r");
	let hcr_rt = ReturnType::new(&Type::Builtin(Native::Integer), hc_retval);
	let mut hcreate = Function::new("hcreate_r", &hcr_rt, &hcreate_args);

//       int hsearch_r(ENTRY item, ACTION action, ENTRY **retval,
//                     struct hsearch_data *htab);

	let item = variable::Source::free("item", &entry.clone(), "", &generators);
	let actvar = variable::Source::free("action", &action, "std:enum:ACTION",
	                                    &generators);
	let entryp = Type::Pointer(Box::new(entry.clone()));
	let rv = variable::Source::free("retval", &entryp, "", &generators);
	let rvexpr = Expression::Simple(ScalarOp::AddressOf, rv.borrow().clone());
	let hsearch_r_args = vec![
		Argument::new(&entry, item),
		Argument::new(&action, actvar),
		Argument::newexpr(&entryp, &rvexpr),
		Argument::newexpr(&hs_data_ptr, &hsd_addr),
	];
	let hs_rv = variable::Source::retval("hserr", "hsearch_r");
	let mut hsearch = Function::new("hsearch_r",
		&ReturnType::new(&Type::Builtin(Native::Integer),	hs_rv),
		&hsearch_r_args
	);

	let mut functions: Vec<&mut Function> = vec![&mut hcreate, &mut hsearch];
	let immut = unsafe {
		// &Vec<&mut T> doesn't coerce to &Vec<&T>.  Really.
		mem::transmute::<&Vec<&mut Function>, &Vec<&Function>>(&functions)
	};

	{
		let nstates = immut.iter().fold(1, |n: usize, ref fqn| {
			let narg = fqn.arguments.iter().fold(1, |na: usize, ref arg| {
				return na*arg.source().generator.n_state();
			});
			return n*narg;
		});
		println!("{} states to test.", nstates);
	}

	while !finished(&immut) {
		//println!("--------- ITERATION {} --------", i);
		//state(&mut std::io::stdout(), &immut);
		match compile_and_test(&mut functions) {
			Err(e) => panic!("compile/test error: {}", e),
			Ok(_) => {},
		};
		next(&mut functions);
	}
	// We next()ed, but then our iteration finished() before we actually
	// compile_and_test()ed the resultant state.  Test that last state.
	match compile_and_test(&mut functions) {
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

	#[test]
	fn parse_stdgen() {
		let mut generators = parse_generators("./share/stdgen.hf"); // todo search
		assert!(generators.len() > 0);
		generators.append(&mut builtin_generators());
		generators.append(&mut hash_generators());
	}

	#[test]
	fn test_iter() {
		let generators = generators_for_test();
		let hs_data = Type::Struct("struct hsearch_data".to_string(), vec![]);

		use variable::ScalarOp;
		let nel = variable::Source::free("nel", &Type::Builtin(Native::Usize),
		                                 "std:usize", &generators);
		let genname = "std:opaque:struct hsearch_data*";
		let hsd_var = variable::Source::free("tbl", &hs_data, genname,
		                                     &generators);
		let hsd_addr = Expression::Simple(ScalarOp::AddressOf,
		                                  hsd_var.borrow().clone());
		let fa1 = Argument::new(&Type::Builtin(Native::Usize), nel);
		let fa2 = Argument::newexpr(&hs_data, &hsd_addr);
		let hcreate_args = vec![fa1, fa2];

		let hc_retval = variable::Source::retval("crterr", "hcreate_r");
		let hcr_rt = ReturnType::new(&Type::Builtin(Native::Integer), hc_retval);
		let mut hcreate = Function::new("hcreate_r", &hcr_rt, &hcreate_args);

		let mut functions: Vec<&mut Function> = vec![&mut hcreate];
		let immut = unsafe {
			// &Vec<&mut T> doesn't coerce to &Vec<&T>.  Really.
			mem::transmute::<&Vec<&mut Function>, &Vec<&Function>>(&functions)
		};
		while !finished(&immut) {
			next(&mut functions);
		}
	}

	#[test]
	fn nonzero_num_iter() {
		let generators = generators_for_test();
		let hs_data = Type::Struct("struct hsearch_data".to_string(), vec![]);

		use variable::ScalarOp;
		let nel = variable::Source::free("nel", &Type::Builtin(Native::Usize),
		                                 "std:usize", &generators);
		let genname = "std:opaque:struct hsearch_data*";
		let hsd_var = variable::Source::free("tbl", &hs_data, genname, &generators);
		let hsd_addr = Expression::Simple(ScalarOp::AddressOf,
		                                  hsd_var.borrow().clone());

		let fa1 = Argument::new(&Type::Builtin(Native::Usize), nel);
		let fa2 = Argument::newexpr(&hs_data, &hsd_addr);
		let hcreate_args = vec![fa1, fa2];

		let hc_retval = variable::Source::retval("crterr", "hcreate_r");
		let hcr_rt = ReturnType::new(&Type::Builtin(Native::Integer), hc_retval);
		let mut hcreate = Function::new("hcreate_r", &hcr_rt, &hcreate_args);

		let functions: Vec<&mut Function> = vec![&mut hcreate];
		let immut = unsafe {
			// &Vec<&mut T> doesn't coerce to &Vec<&T>.  Really.
			mem::transmute::<&Vec<&mut Function>, &Vec<&Function>>(&functions)
		};

		let nstates = immut.iter().fold(1, |n: usize, ref fqn| {
			let narg = fqn.arguments.iter().fold(1, |na: usize, ref arg| {
				return na*arg.source().generator.n_state();
			});
			return n*narg;
		});
		assert_eq!(nstates, 4);
	}

	#[test]
	fn compile_generated_program() {
		let generators = generators_for_test();
		let hs_data = Type::Struct("struct hsearch_data".to_string(), vec![]);

		use variable::ScalarOp;
		let nel = variable::Source::free("nel", &Type::Builtin(Native::Usize),
		                                 "std:usize", &generators);
		let genname = "std:opaque:struct hsearch_data*";
		let hsd_var = variable::Source::free("tbl", &hs_data, genname,
		                                     &generators);
		let hsd_addr = Expression::Simple(ScalarOp::AddressOf,
		                                  hsd_var.borrow().clone());
		let fa1 = Argument::new(&Type::Builtin(Native::Usize), nel);
		let fa2 = Argument::newexpr(&hs_data, &hsd_addr);
		let hcreate_args = vec![fa1, fa2];

		let hc_retval = variable::Source::retval("crterr", "hcreate_r");
		let hcr_rt = ReturnType::new(&Type::Builtin(Native::Integer), hc_retval);
		let mut hcreate = Function::new("hcreate_r", &hcr_rt, &hcreate_args);

		let mut functions: Vec<&mut Function> = vec![&mut hcreate];
		match compile_and_test(&mut functions) {
			Err(e) => panic!("compile/test error: {}", e),
			Ok(_) => {},
		};
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
		let fname = ".parse_hash_decls_tmp";
		dump(fname, &s);

		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let types = parse_types(fname, &mut generators);
		match std::fs::remove_file(fname) {
			Err(e) => panic!("error removing {:?}: {}", fname, e),
			_ => (),
		};
		assert_eq!(types.len(), 3);
		for t in types {
			println!("\t{:?}", t);
		}
		// todo: should assert properties of those types
	}

	#[test]
	fn hash_table_search_case() {
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
		assert_eq!(lprogram.statements.len(), 2);
		match lprogram.analyze() {
			Err(e) => panic!(e),
			_ => (),
		};
		match compile_and_test_program(&lprogram) {
			Err(e) => panic!(e),
			Ok(_) => (),
		};
	}
}
