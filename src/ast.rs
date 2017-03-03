// This houses our AST types for parsing generator definitions.
extern crate rand;
use std::fmt;
use rand::distributions::{IndependentSample, Range};

use typ::*;
use variable::Generator;

#[derive(Clone)]
pub enum Opcode {
	Add, Sub, Mul, Div, Mod,
}

pub struct UserGen {
	ty: Type,
	pub name: String,
	states: Vec<Expression>,
	idx: usize,
	rng: rand::ThreadRng,
}
impl UserGen {
	pub fn new(t: Type, nm: &String, stlist: &Vec<Expression>) -> Self {
		UserGen{ty: t, name: nm.clone(), states: (*stlist).clone(),
		        idx: 0, rng: rand::thread_rng()}
	}

	fn typed_min(t: &Type) -> String {
		match t {
			&Type::I8 => i8::min_value().to_string(),
			&Type::U8 => u8::min_value().to_string(),
			&Type::I16 => i16::min_value().to_string(),
			&Type::U16 => u16::min_value().to_string(),
			&Type::I32 => i32::min_value().to_string(),
			&Type::U32 => u32::min_value().to_string(),
			&Type::I64 => i64::min_value().to_string(),
			&Type::U64 => u64::min_value().to_string(),
			&Type::Usize => usize::min_value().to_string(),
			&Type::Integer => i32::min_value().to_string(),
			_ => panic!("No minimum for '{:?}'", t),
		}
	}
	fn typed_max(t: &Type) -> String {
		match t {
			&Type::I8 => i8::max_value().to_string(),
			&Type::U8 => u8::max_value().to_string(),
			&Type::I16 => i16::max_value().to_string(),
			&Type::U16 => u16::max_value().to_string(),
			&Type::I32 => i32::max_value().to_string(),
			&Type::U32 => u32::max_value().to_string(),
			&Type::I64 => i64::max_value().to_string(),
			&Type::U64 => u64::max_value().to_string(),
			&Type::Usize => usize::max_value().to_string(),
			&Type::Integer => i32::max_value().to_string(),
			_ => panic!("No minimum for '{:?}'", t),
		}
	}

	fn interp(&mut self, expr: &Expression) -> String {
		match expr {
			&Expression::ConstExpr(ref c) => c.to_string(),
			&Expression::Compound(ref left, ref op, ref right) => {
				let l = self.interp(left);
				let r = self.interp(right);
				// Hack: we assume i64 is sufficient for everything ...
				let lhs = l.parse::<i64>().unwrap();
				let rhs = r.parse::<i64>().unwrap();
				let result = match op {
					&Opcode::Add => lhs + rhs,
					&Opcode::Sub => lhs - rhs,
					&Opcode::Mul => lhs * rhs,
					&Opcode::Div => lhs / rhs,
					&Opcode::Mod => lhs % rhs,
				};
				result.to_string()
			},
			&Expression::MinExpr(ref typ) => UserGen::typed_min(&typ),
			&Expression::MaxExpr(ref typ) => UserGen::typed_max(&typ),
			&Expression::RandomExpr(ref typ, ref low, ref high) => {
				println!("fixme: proper typing to {:?}", typ);
				let l = self.interp(low);
				let h = self.interp(high);
				// Hack: we assume i64 is sufficient for everything ...
				let lo = l.parse::<i64>().unwrap();
				let hi = h.parse::<i64>().unwrap();
				let range = Range::new(lo, hi);
				range.ind_sample(&mut self.rng).to_string()
			}
		}
	}
}
impl fmt::Debug for UserGen {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.dbg(f)
	}
}

#[derive(Clone)]
pub enum Expression {
	ConstExpr(Constant),
	Compound(Box<Expression>, Opcode, Box<Expression>),
	MinExpr(Type), // i.e. "i32:min()"
	MaxExpr(Type),
	RandomExpr(Type, Box<Expression>, Box<Expression>), // exprs are low and high
}
impl fmt::Debug for Expression {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&Expression::ConstExpr(ref expr) => write!(f, "Constant({:?})", expr),
			&Expression::Compound(ref left, Opcode::Add, ref right) =>
				write!(f, "{:?} + {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Sub, ref right) =>
				write!(f, "{:?} - {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Mul, ref right) =>
				write!(f, "{:?} * {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Div, ref right) =>
				write!(f, "{:?} / {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Mod, ref right) =>
				write!(f, "{:?} % {:?}", left, right),
			&Expression::MinExpr(ref ty) => write!(f, "{}:min()", ty.name()),
			&Expression::MaxExpr(ref ty) => write!(f, "{}:max()", ty.name()),
			&Expression::RandomExpr(ref ty, ref low, ref high) =>
				write!(f, "{}:random[{:?}, {:?})", ty.name(), low, high),
		}
	}
}

// All of the integer types get annoying, so we just use one [un]signed64.
#[derive(Clone)]
pub enum Constant {
	Signed(i64),
	Unsigned(u64),
	String(String)
}
impl fmt::Display for Constant {
	fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
		match self {
			&Constant::Signed(ref v) => write!(f, "{}", v),
			&Constant::Unsigned(ref v) => write!(f, "{}", v),
			&Constant::String(ref v) => write!(f, "{}", v),
		}
	}
}
impl fmt::Debug for Constant {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&Constant::Signed(v) => write!(f, "Signed{{{}}}", v),
			&Constant::Unsigned(v) => write!(f, "Unsigned{{{}}}", v),
			&Constant::String(ref s) => write!(f, "String{{{}}}", s),
		}
	}
}

	//pub ty: Type,
	//pub states: Vec<Expression>,
impl ::variable::Generator for UserGen {
	fn value(&mut self) -> String {
		let i = self.idx;
		let expr: Expression = self.states[i].clone();
		self.interp(&expr)
	}
	fn next(&mut self) {
		if self.idx < self.states.len()-1 {
			self.idx = self.idx + 1;
		}
	}
	fn done(&self) -> bool { self.idx >= self.states.len()-1 }
	fn n_state(&self) -> usize { self.states.len() }
	fn reset(&mut self) { self.idx = 0; }

	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{}{{{:?}, state {} of {}: {{\n", self.name, self.ty,
		       self.idx, self.states.len()).unwrap();
		for state in self.states.iter() {
			write!(f, "\t{:?}\n", state).unwrap();
		}
		write!(f, "}}}}")
	}
}
