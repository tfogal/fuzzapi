use std::collections::btree_map::BTreeMap;
use std::fmt::Write;
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

trait Name {
	fn name(&self) -> String;
}

impl Name for Type {
	fn name<'a>(&'a self) -> String {
		use typ::Type::*;
		let mut res = String::new();
		match self {
			&U8 => "uint8_t",
			&I8 => "int8_t",
			&U16 => "uint16_t", &I16 => "int16_t",
			&U32 => "uint32_t", &I32 => "int32_t",
			&U64 => "uint64_t", &I64 => "int64_t",
			&F32 => "float", &F64 => "double",
			&Usize => "size_t", &Integer => "int", &Unsigned => "unsigned",
			&Character => "char",
			&Void => "void",
			&Enum(ref enm, _) => {
				tryp!(write!(&mut res, "enum {}", enm));
				res.as_str().clone()
			},
			&Type::UDT(ref udt, _) => udt,
			&Type::Field(_, ref ty) => {
				tryp!(write!(&mut res, "{}", ty.name()));
				res.as_str().clone()
			},
			&Type::Pointer(ref t) => {
				tryp!(write!(&mut res, "{}*", t.name()));
				res.as_str().clone()
			},
		}.to_string()
	}
}

/* todo this is outdated and broken */
struct ValueU64 {
	tested: bloom::Bloom,
	rng: rand::ThreadRng,
	distr: rand::distributions::Range<u64>,
}
impl ValueU64 {
	pub fn new() -> Self {
		ValueU64 {
			tested: bloom::Bloom::new(),
			rng: rand::thread_rng(),
			distr: rand::distributions::Range::new(u64::min_value(), u64::max_value()),
		}
	}
	pub fn get(&mut self) -> u64 {
		use rand::distributions::IndependentSample;
		let mut sample = self.distr.ind_sample(&mut self.rng);
		let mut i: usize = 0;
		while self.tested.query(sample) {
			sample = self.distr.ind_sample(&mut self.rng);
			// 'complete' is expensive so we don't check every time.
			i = i+1;
			if i >= 512 && self.tested.complete() {
				break;
			}
		}
		self.tested.add(sample);
		return sample;
	}
}

// A dependent variable is a variable that we don't actually have control over.
// For example, if the API model states that 'the return value of f() must be
// the second argument of g()', a la:
//   type v = f();
//   g(_, v);
// Then 'v' is dependent.  The effect is mostly that we don't attach a Value to
// it.
#[allow(dead_code)]
struct DependentVariable<'a> {
	name: String,
	src: variable::Source<'a>,
	dest: variable::Use<'a>,
	// do we need a type: Type ?
}

// A free variable is a variable that we DO have control over.  This generally
// means variables that are API inputs.
#[allow(dead_code)]
// todo this is broken, but the idea of a u64 free var should be in variable.rs
struct FreeVariableU64<'a> {
	name: String,
	tested: ValueU64, // probably want to parametrize
	dest: variable::Use<'a>,
	ty: &'a Type,
}

fn prototypes(strm: &mut std::io::Write, functions: &Vec<&Function>) {
	for fqn in functions.iter() {
		tryp!(write!(strm, "extern {} {}(", fqn.return_type.name(), fqn.name));
		for a in 0..fqn.arguments.len() {
			tryp!(write!(strm, "{}", fqn.arguments[a].name()));
			if a != fqn.arguments.len()-1 {
				tryp!(write!(strm, ", "));
			}
		}
		tryp!(writeln!(strm, ");"));
	}
}

fn generate(mut strm: &mut std::io::Write, functions: &Vec<&Function>,
            v64: &mut ValueU64) {
	tryp!(writeln!(strm, "#include <stdio.h>"));
	tryp!(writeln!(strm, "#include <stdlib.h>"));
	tryp!(writeln!(strm, "#include <search.h>\n"));
	prototypes(&mut strm, functions);
	tryp!(writeln!(strm, "\nint main() {{"));

	// produce variable declarations of the form '<Type> vYY = 0;'.
	let mut i: usize = 0;
	for fqn in functions.iter() {
		for a in 0..fqn.arguments.len() {
			let value = v64.get();
			// if it's a pointer, deref once to create the correct kind of thing.
			// then when we pass it, we'll pass "&it" instead of just "it".
			match &fqn.arguments[a] {
				&Type::Pointer(ref t) => {
					tryp!(writeln!(strm, "\t{} v{}{};", t.name(), a, i));
				},
				ref t => {
					tryp!(writeln!(strm, "\t{} v{}{} = {};", t.name(), a, i, value));
				},
			};
		}
		i += 1;
	}

	/* produce "x = f(...);" lines. */
	for f in 0..functions.len() {
		tryp!(write!(strm, "\t{} frv{} = {}(", functions[f].return_type.name(),
		             f, functions[f].name));
		for a in 0..functions[f].arguments.len() {
			match &functions[f].arguments[a] {
				&Type::Pointer(ref _t) => tryp!(write!(strm, "&v{}{}", a, f)),
				ref _t => tryp!(write!(strm, "v{}{}", a, f)),
			};
			if a != functions[f].arguments.len()-1 {
				tryp!(write!(strm, ", "));
			}
		}
		tryp!(writeln!(strm, ");"));
	}
	tryp!(writeln!(strm, "\treturn EXIT_SUCCESS;"));
	tryp!(writeln!(strm, "}}"));
}

// An API is a collection of Functions, DependentVariables, and FreeVariables.
struct API<'a> {
	fqn: &'a Vec<Function>,
	free: &'a Vec<Box<variable::Free>>,
	dep: &'a Vec<Box<DependentVariable<'a>>>,
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

fn compile(src: &str, dest: &str) -> Result<(), String> {
	// todo/fixme: don't hardcode arguments, read from config file or similar.
	let compile = match Command::new("gcc").arg("-Wall").arg("-Wextra")
	                                       .arg("-fcheck-pointer-bounds")
	                                       .arg("-mmpx")
	                                       .arg("-D_GNU_SOURCE").arg("-o")
	                                       .arg(dest).arg(src).output() {
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
	}
	Ok(())
}

fn main() {
	let hs_data = Type::UDT("struct hsearch_data".to_string(), vec![]);
	let hs_data_ptr: Type = Type::Pointer(Box::new(hs_data.clone()));
	let hcreate_r_args = vec![Type::Usize, hs_data_ptr];
	let hcreate_r  = Function { return_type: Type::Integer,
	                            arguments: hcreate_r_args,
	                            name: "hcreate_r".to_string() };
	let entry = Type::UDT("ENTRY".to_string(),
		vec![Box::new(Type::Pointer(Box::new(Type::Character))),
		     Box::new(Type::Pointer(Box::new(Type::Void)))]
	);
	let mut action_values: BTreeMap<String, u32> = BTreeMap::new();
	action_values.insert("FIND".to_string(), 0);
	action_values.insert("ENTER".to_string(), 1);
	let action = Type::Enum("ACTION".to_string(), action_values);

	let hs_args: Vec<Type> = vec![
		entry.clone(),
		action,
		Type::Pointer(Box::new(Type::Pointer(Box::new(entry)))),
		Type::Pointer(Box::new(hs_data))
	];
	let hsrch = Function { return_type: Type::Integer, arguments: hs_args,
	                       name: "hsearch_r".to_string() };

	{
		let fqns: Vec<Function> = vec![hcreate_r.clone(), hsrch.clone()];
		let mut depvar: Vec<Box<DependentVariable>> = Vec::new();
		let mut freevar: Vec<Box<variable::Free>> = Vec::new();
		depvar.push(Box::new(DependentVariable{
			name: "tbl".to_string(),
			src: variable::Source::Parameter(&fqns[0], 1),
			dest: variable::Use::Argument(&fqns[1], 3),
		}));
		freevar.push(Box::new(variable::FreeUDT {
			name: "item".to_string(),
			tested: variable::ValueUDT::create(&fqns[1].arguments[0]),
			dest: variable::Use::Argument(&fqns[1], 0),
			ty: &hsrch.arguments[0],
		}));
		freevar.push(Box::new(variable::FreeEnum {
			name: "action".to_string(),
			tested: variable::ValueEnum::create(&fqns[1].arguments[1]),
			dest: variable::Use::Argument(&fqns[1], 1),
			ty: &fqns[1].arguments[1],
		}));
		// return type, but it actually comes from an argument...
		freevar.push(Box::new(variable::FreeI32 {
			name: "retval".to_string(),
			tested: variable::ValueI32::create(&hsrch.arguments[2]),
			dest: variable::Use::Nil,
			ty: &fqns[1].arguments[2],
		}));
		// todo / fixme: add a method that takes an API and generates the "next"
		// program.
	}

	// next:
	// 1) generate a "state vector", which is the cartesian product of all states
	//    of all 'tested' values for all FreeVariables
	// 2) generalize the 'generate' function so that it takes the state vector
	//    and uses it to generate a unique program that tests that particular
	//    state
	// 3) write something that lets you iterate through state vectors, i.e. a
	//    function that takes a state vector and returns the "next" state vector.
	// 4) rewrite the bloom filter stuff for FreeVariable::tested.  What you
	//    really want is just to divide the potential value space of each type
	//    into a set of distinct classes.  The number of states for that variable
	//    is the number of classes defined on that type.
	// => Similarly, you really need a whole set of types instead of trying to
	//    force 'ValueU64' on everything.


	let fname: &'static str = "/tmp/fuzzapi.c";
	let mut f = match File::create(fname) {
		Err(e) => {
			println!("Could not create {}: {}", fname, e);  /* FIXME stderr */
			std::process::exit(1);
		},
		Ok(x) => x,
	};
	let mut used = ValueU64::new();
	generate(&mut f, &vec![&hcreate_r], &mut used);
	let outname: &'static str = ".fuzziter";
	match compile(fname, outname) {
		Err(x) => panic!(x), Ok(_) => {},
	};

	let cmdname: String = String::from("./") + String::from(outname).as_str();
	if system(cmdname).is_err() {
		panic!("Might have found a bug, exiting ...");
	}
}
