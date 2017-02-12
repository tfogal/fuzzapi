use std::rc::Rc;
use std::collections::btree_map::BTreeMap;
use std::fs::File;
use std::process::Command;
extern crate rand;
mod bitvector;
mod bloom;
mod function;
mod tc;
mod typ;
mod variable;
use function::*;
use typ::*;

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

// Just an example of how to use the Function API.  This will print out a
// prototype for every function in the list.
#[allow(dead_code)]
fn prototypes(strm: &mut std::io::Write, functions: &Vec<Function>) {
	for fqn in functions.iter() {
		let (ref ty, _) = fqn.return_type;
		tryp!(write!(strm, "extern {} {}(", ty.name(), fqn.name));
		for a in 0..fqn.arguments.len() {
			let (ref argtype, _) = fqn.arguments[a];
			tryp!(write!(strm, "{}", argtype.name()));
			if a != fqn.arguments.len()-1 {
				tryp!(write!(strm, ", "));
			}
		}
		tryp!(writeln!(strm, ");"));
	}
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

// Prints out the variable name as it would be used in an r-value context.
// Notably this includes any scalar operations that need to be applied in the
// given context.
fn rvalue(src: &variable::Source) -> String {
	use std::fmt::Write;
	let mut rv = String::new();

	match src {
		&variable::Source::Free(ref nm, _, ref op) => {
			tryp!(write!(rv, "{}{}", op.to_string(), nm));
		},
		&variable::Source::Return(_, _) => {
			tryp!(write!(rv, "/*fixme, from ret*/"));
		},
		&variable::Source::Parent(ref spar, _) => {
			// should we be prepending the ScalarOp to whatever the recursion gives?
			return rvalue(spar);
		},
	}
	return rv;
}

fn gen(strm: &mut std::io::Write, fqns: &Vec<Function>) ->
	std::io::Result<()> {
	let hdrs: Vec<&str> = vec!["search.h"];
	try!(header(strm, &hdrs));
	try!(writeln!(strm, "")); // just a newline to separate them out.

	//prototypes(strm, fqns);
	// todo: we need to derive some ordering for functions so that we can do this
	// properly. for now we just decree that the functions are given in the order
	// that it makes sense to call them...

	try!(writeln!(strm, "int main() {{"));

	use std::ops::Deref;
	for fqn in fqns {
		// declare all variables used as arguments to the function
		for ref arg in fqn.arguments.iter() {
			let &(ref argtype, ref src) = *arg;
			match src.deref() {
				&variable::Source::Free(ref nm, ref gen, _) => {
					try!(writeln!(strm, "\t{} {} = {};", argtype.name(), nm, gen.get()));
				},
				_ => {} // all vars eventually come from a free
			}
		};
		let (ref rettype, ref src) = fqn.return_type;
		try!(write!(strm, "\t{} {} = {}(", rettype.name(), src.name(), fqn.name));

		for (a, ref arg) in fqn.arguments.iter().enumerate() {
			let &(_, ref src) = *arg;
			try!(write!(strm, "{}", rvalue(src.deref())));
			if a < fqn.arguments.len()-1 {
				try!(write!(strm, ", "));
			}
		}
		try!(writeln!(strm, ");"));
	}

	try!(writeln!(strm, "}}"));
	return Ok(());
}

fn system(cmd: String) -> Result<(), std::io::Error> {
	let run = match Command::new(cmd).output() {
		Err(e) => {
			println!("Program error: {}", e); // FIXME stderr
			// cat 'run' ...
			return Err(e);
		},
		Ok(x) => x,
	};
	let runout: String = String::from_utf8(run.stdout).unwrap();
	if runout.len() > 0 {
		println!("program output: '{}'", runout);
	}
	return Ok(());
}

fn compile(src: &str, dest: &str, flags: &Vec<&str>) -> Result<(), String> {
	// todo/fixme: don't hardcode arguments, read from config file or similar.
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
			let s = fmt::format(format_args!("compilation of {} failed: {}", src, e));
			return Err(s);
		},
		Ok(x) => x,
	};
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

fn main() {
	let hs_data = Type::UDT("struct hsearch_data".to_string(), vec![]);
	let hs_data_ptr: Type = Type::Pointer(Box::new(hs_data.clone()));
	let char_ptr = Type::Pointer(Box::new(Type::Character));
	let void_ptr = Type::Pointer(Box::new(Type::Void));
	let entry = Type::UDT("ENTRY".to_string(),
		vec![Box::new(Type::Field("key".to_string(), Box::new(char_ptr))),
		     Box::new(Type::Field("data".to_string(), Box::new(void_ptr)))]
	);
	let mut action_values: BTreeMap<String, u32> = BTreeMap::new();
	action_values.insert("FIND".to_string(), 0);
	action_values.insert("ENTER".to_string(), 1);
	let action = Type::Enum("ACTION".to_string(), action_values);

	let fname: &'static str = "/tmp/fuzziter.c";
	{
		use variable::ScalarOp;
		let nel = variable::Source::Free("nel".to_string(),
		                                 variable::generator(&Type::Usize),
		                                 ScalarOp::Null);
		let hsd_var = Rc::new(
			variable::Source::Free("tbl".to_string(), variable::generator(&hs_data),
			                       ScalarOp::AddressOf)
		);
		let fa1: function::Argument = (Type::Usize, Rc::new(nel));
		let fa2: function::Argument = (hs_data.clone(), hsd_var.clone());
		let hcreate_args = vec![fa1, fa2];

		let hc_retval = variable::Source::Free("crterr".to_string(),
		                                       variable::generator(&Type::I32),
		                                       ScalarOp::Null);
		let hcreate = function::Function{
			return_type: (Type::Integer, Rc::new(hc_retval)),
			arguments: hcreate_args,
		  name: "hcreate_r".to_string(),
		};

//       int hsearch_r(ENTRY item, ACTION action, ENTRY **retval,
//                     struct hsearch_data *htab);
		let item = variable::Source::Free("item".to_string(),
		                                  variable::generator(&entry.clone()),
		                                  ScalarOp::Null);
		let actvar = variable::Source::Free("action".to_string(),
		                                    variable::generator(&action),
		                                    ScalarOp::Null);
		let entryp = Type::Pointer(Box::new(entry.clone()));
		let rv = variable::Source::Free("retval".to_string(),
		                                variable::generator(&entryp),
		                                ScalarOp::AddressOf);
		let htab = variable::Source::Parent(hsd_var.clone(), ScalarOp::AddressOf);
		let hsearch_r_args = vec![
			(entry, Rc::new(item)),
			(action, Rc::new(actvar)),
			(entryp, Rc::new(rv)),
			(hs_data_ptr.clone(), Rc::new(htab)),
		];
		let hs_rv = variable::Source::Free("hserr".to_string(),
		                                   variable::generator(&Type::I32),
		                                   ScalarOp::Null);
		let hsearch = function::Function{
			return_type: (Type::Integer, Rc::new(hs_rv)),
			arguments: hsearch_r_args,
			name: "hsearch_r".to_string(),
		};

		let mut newtest = match File::create(fname) {
			Err(e) => {
				println!("Could not create {}: {}", fname, e); /* FIXME stderr */
				std::process::exit(1);
			},
			Ok(x) => x,
		};
		match gen(&mut newtest, &vec![hcreate, hsearch]) {
			Err(x) => panic!(x),
			Ok(_) => {},
		};
	}
	let outname: &'static str = ".fuzziter";
	let args = vec!["-Wall", "-Wextra", "-fcheck-pointer-bounds", "-mmpx",
	                "-D_GNU_SOURCE"];
	match compile(fname, outname, &args) {
		Err(x) => panic!(x), Ok(_) => {},
	};

	let cmdname: String = String::from("./") + String::from(outname).as_str();
	if system(cmdname).is_err() {
		panic!("Might have found a bug, exiting ...");
	}
}
