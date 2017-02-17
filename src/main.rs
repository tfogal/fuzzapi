use std::collections::btree_map::BTreeMap;
use std::fs::File;
use std::process::Command;
extern crate rand;
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
		let ref ty = fqn.return_type.ty;
		tryp!(write!(strm, "extern {} {}(", ty.name(), fqn.name));
		for a in 0..fqn.arguments.len() {
			let ref argtype = fqn.arguments[a].ty;
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

	if src.is_free() {
		tryp!(write!(rv, "{}{}", src.op.to_string(), src.name()));
	} else if src.is_bound() {
		/* should we prepend scalar op? */
		use std::ops::Deref;
		return rvalue(src.parent[0].borrow().deref());
	} else if src.is_retval() {
		tryp!(write!(rv, "/*fixme, from ret*/"));
	}
	return rv;
}

fn reset_args(fqn: &mut Function) {
	use std::ops::DerefMut;
	for arg in fqn.arguments.iter() {
		if arg.src.borrow().is_free() {
			arg.src.borrow_mut().generator.deref_mut().reset();
		}
	}
}

fn fqnfinished(func: &Function) -> bool {
	if func.arguments.iter().all( // all ...
		|ref a| a.src.borrow().generator.done() // ... done
	) {
		return true;
	}
	return false;
}

fn finished(functions: &Vec<&Function>) -> bool {
	if functions.iter().all(|ref f| fqnfinished(f)) {
		return true;
	}
	return false;
}

fn gen(strm: &mut std::io::Write, fqns: &Vec<&Function>) -> std::io::Result<()> {
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
			//let &(ref argtype, ref src) = *arg;
			if arg.src.borrow().is_free() {
				try!(writeln!(strm, "\t{} {} = {};", arg.ty.name(), arg.src.borrow().name(),
				              arg.src.borrow().generator.get()));
			} // no else: all vars eventually come from a free
		};
		let ref ret = fqn.return_type;
		try!(write!(strm, "\t{} {} = {}(", ret.ty.name(), ret.src.borrow().name(), fqn.name));

		for (a, ref arg) in fqn.arguments.iter().enumerate() {
			//let &(_, ref src) = *arg;
			try!(write!(strm, "{}", rvalue(arg.src.deref().borrow().deref())));
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
		let nel = variable::Source::free("nel", &Type::Usize, ScalarOp::Null);
		let hsd_var = variable::Source::free("tbl", &hs_data, ScalarOp::AddressOf);
		let fa1 = Argument::new(&Type::Usize, nel);
		let fa2 = Argument::new(&hs_data, hsd_var.clone());
		let hcreate_args = vec![fa1, fa2];

		let hc_retval = variable::Source::free("crterr", &Type::I32,
		                                       ScalarOp::Null);
		let hcr_rt = ReturnType::new(&Type::Integer, hc_retval);
		let mut hcreate = Function::new("hcreate_r", &hcr_rt, &hcreate_args);

//       int hsearch_r(ENTRY item, ACTION action, ENTRY **retval,
//                     struct hsearch_data *htab);
		let item = variable::Source::free("item", &entry.clone(), ScalarOp::Null);
		let actvar = variable::Source::free("action", &action, ScalarOp::Null);
		let entryp = Type::Pointer(Box::new(entry.clone()));
		let rv = variable::Source::free("retval", &entryp, ScalarOp::AddressOf);
		let htab = variable::Source::bound(hsd_var.clone(), ScalarOp::AddressOf);
		let hsearch_r_args = vec![
			Argument::new(&entry, item),
			Argument::new(&action, actvar),
			Argument::new(&entryp, rv),
			Argument::new(&hs_data_ptr, htab),
		];
		let hs_rv = variable::Source::free("hserr", &Type::Integer, ScalarOp::Null);
		let mut hsearch = Function::new("hsearch_r",
			&ReturnType::new(&Type::Integer,	hs_rv), &hsearch_r_args);

		let mut newtest = match File::create(fname) {
			Err(e) => {
				println!("Could not create {}: {}", fname, e); /* FIXME stderr */
				std::process::exit(1);
			},
			Ok(x) => x,
		};
		{
		let mut functions: Vec<&mut Function> = vec![&mut hcreate, &mut hsearch];
		}
		// We can't just use our vector of mutable functions because vectors of
		// mutable things can't coerce to vectors of immutable things.  Really.
		let hate_rust: Vec<&Function> = vec![&hcreate, &hsearch];
		match gen(&mut newtest, &hate_rust) {
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
