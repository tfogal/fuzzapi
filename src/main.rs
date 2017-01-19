use std::fmt::Write;
use std::fs::File;
use std::process::Command;
extern crate rand;
use rand::Rng;
mod bitvector;
mod bloom;

#[derive(PartialEq)]
#[allow(dead_code)]
enum Type {
	U8, I8, U16, I16, U32, I32, U64, I64, F32, F64,
	USIZE, INTEGER, UNSIGNED,
	UDT(String, Box<Type>),
	Pointer(Box<Type>),
	/* Nil is a sentinel to end recursion, specifically for UDTs.  For example,
	 * "struct ty;" is an opaque type, so its typelist is just 'Nil'. */
	Nil,
}
trait Name {
	fn name(&self) -> String;
}

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

impl Name for Type {
	fn name(&self) -> String {
		use Type::*;
		let mut res = String::new();
		match self {
			&U8 => "uint8_t",
			&I8 => "int8_t",
			&U16 => "uint16_t", &I16 => "int16_t",
			&U32 => "uint32_t", &I32 => "int32_t",
			&U64 => "uint64_t", &I64 => "int64_t",
			&F32 => "float", &F64 => "double",
			&USIZE => "size_t", &INTEGER => "int", &UNSIGNED => "unsigned",
			&Type::UDT(ref udt, _) => udt,
			&Type::Pointer(ref t) => {
				tryp!(write!(&mut res, "{}*", t.name()));
				res.as_str().clone()
			},
			&Nil => "bug; type is nil!"
		}.to_string()
	}
}

struct Function {
	return_type: Type,
	arguments: Vec<Type>,
	name: String,
}
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
		while self.tested.query(sample) {
			sample = self.distr.ind_sample(&mut self.rng);
		}
		self.tested.add(sample);
		return sample;
	}
}

fn prototypes(strm: &mut std::io::Write, functions: &Vec<Function>) {
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

fn generate(mut strm: &mut std::io::Write, functions: &Vec<Function>,
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

fn main() {
	let hs_data = Type::UDT("struct hsearch_data".to_string(), Box::new(Type::Nil));
	let hs_data_ptr: Type = Type::Pointer(Box::new(hs_data));
	let hcreate_r_args = vec![Type::USIZE, hs_data_ptr];
	let hcreate_r  = Function { return_type: Type::INTEGER,
	                            arguments: hcreate_r_args,
	                            name: "hcreate_r".to_string() };
	let fname: &'static str = "/tmp/fuzzapi.c";
	let mut f = match File::create(fname) {
		Err(e) => {
			println!("Could not create {}: {}", fname, e);  /* FIXME stderr */
			std::process::exit(1);
		},
		Ok(x) => x,
	};
	let mut used = ValueU64::new();
	generate(&mut f, &vec![hcreate_r], &mut used);
	let outname: &'static str = ".fuzziter";
	let output = Command::new("gcc").arg("-Wall").arg("-Wextra")
	                                .arg("-fcheck-pointer-bounds")
	                                .arg("-mmpx")
	                                .arg("-D_GNU_SOURCE").arg("-o")
	                                .arg(outname).arg(fname).output();
	let output = match output {
		Err(e) => {
			println!("compilation failed: {}", e); /* FIXME stderr */
			panic!("");
		},
		Ok(x) => x,
	};
	let outs: String = String::from_utf8(output.stdout).unwrap();
	println!("gcc output: '{}'", outs);
}
