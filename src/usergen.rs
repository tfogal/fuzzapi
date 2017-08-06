// This houses our types from parsed generator definitions.
extern crate rand;
use std::fmt;
use rand::distributions::{IndependentSample, Range};

use opcode::Opcode;
use typ::*;
use variable::Generator;

#[derive(Clone)]
pub struct UserGen {
	ty: Type,
	pub name: String,
	states: Vec<Expression>,
	idx: usize,
}
impl UserGen {
	pub fn new(t: Type, nm: &String, stlist: &Vec<Expression>) -> Self {
		UserGen{ty: t, name: nm.clone(), states: (*stlist).clone(),
		        idx: 0}
	}

	fn typed_min(t: &Type) -> String {
		let ty = match t {
			&Type::Builtin(ref blt) => blt.clone(),
			_ => panic!("need native type for typed_min: {:?}", t),
		};
		match ty {
			Native::I8 => i8::min_value().to_string(),
			Native::U8 => u8::min_value().to_string(),
			Native::I16 => i16::min_value().to_string(),
			Native::U16 => u16::min_value().to_string(),
			Native::I32 => i32::min_value().to_string(),
			Native::U32 => u32::min_value().to_string(),
			Native::I64 => i64::min_value().to_string(),
			Native::U64 => u64::min_value().to_string(),
			Native::Usize => usize::min_value().to_string(),
			Native::Integer => i32::min_value().to_string(),
			_ => panic!("No minimum for '{:?}'", t),
		}
	}
	fn typed_max(t: &Type) -> String {
		let ty = match t {
			&Type::Builtin(ref blt) => blt.clone(),
			_ => panic!("need native type for typed_max: {:?}", t),
		};
		match ty {
			Native::I8 => i8::max_value().to_string(),
			Native::U8 => u8::max_value().to_string(),
			Native::I16 => i16::max_value().to_string(),
			Native::U16 => u16::max_value().to_string(),
			Native::I32 => i32::max_value().to_string(),
			Native::U32 => u32::max_value().to_string(),
			Native::I64 => i64::max_value().to_string(),
			Native::U64 => u64::max_value().to_string(),
			Native::Usize => usize::max_value().to_string(),
			Native::Integer => i32::max_value().to_string(),
			_ => panic!("No minimum for '{:?}'", t),
		}
	}

	fn interp(&self, expr: &Expression) -> String {
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
					&Opcode::LAnd => (lhs > 0 && rhs > 0) as i64,
					&Opcode::LOr => (lhs > 0 || rhs > 0) as i64,
					&Opcode::Greater => (lhs > rhs) as i64,
					&Opcode::Less => (lhs < rhs) as i64,
					&Opcode::NotEqual => (lhs != rhs) as i64,
					&Opcode::Equal => (lhs == rhs) as i64,
					&Opcode::Not => unreachable!(),
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
				range.ind_sample(&mut rand::thread_rng()).to_string()
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
			&Expression::Compound(ref left, Opcode::LAnd, ref right) =>
				write!(f, "{:?} && {:?}", left, right),
			&Expression::Compound(ref left, Opcode::LOr, ref right) =>
				write!(f, "{:?} || {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Greater, ref right) =>
				write!(f, "{:?} > {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Less, ref right) =>
				write!(f, "{:?} < {:?}", left, right),
			&Expression::Compound(ref left, Opcode::NotEqual, ref right) =>
				write!(f, "{:?} != {:?}", left, right),
			&Expression::Compound(ref left, Opcode::Equal, ref right) =>
				write!(f, "{:?} == {:?}", left, right),
			&Expression::Compound(_, Opcode::Not, _) =>
				unreachable!(),
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

impl ::variable::Generator for UserGen {
	fn name(&self) -> String { self.name.clone() }
	fn value(&self) -> String {
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
	fn clone(&self) -> Box<Generator> {
		Box::new(UserGen{ty: self.ty.clone(), name: self.name.clone(),
		                 states: self.states.clone(),
		                 idx: self.idx})
	}
}

#[cfg(test)]
mod test {
	use generator;

	#[test]
	fn parse_generator() {
		let s = "generator name I32 state i32:constant(42)";
		assert!(generator::parse_LGeneratorList(s).is_ok());
	}
	#[test]
	fn minexpr() {
		let s = "generator name U8 state u8:min()";
		assert!(generator::parse_LGeneratorList(s).is_ok());
	}
	#[test]
	fn maxexpr() {
		let s = "generator name U16 state u16:max()";
		assert!(generator::parse_LGeneratorList(s).is_ok());
	}

	#[test]
	fn randexpr_constants() {
		let s = "generator name U32 state u32:random(i32:constant(1),";
		let s = s.to_string() + "i32:constant(32768))";
		let t = s.as_str();
		assert!(generator::parse_LGeneratorList(t).is_ok());
	}

	#[test]
	fn randexpr_min_max() {
		let s = "generator name U32 state u32:random(i32:min(), i32:max())";
		assert!(generator::parse_LGeneratorList(s).is_ok());
	}

	#[test]
	fn randexpr_complex() {
		let s =
			"generator name i32\n".to_string() +
			"state i32:random(i32:max() / i32:constant(2), i32:constant(1))";
		match generator::parse_LGeneratorList(s.as_str()) {
			Ok(_) => {},
			Err(e) => panic!("err: {:?}", e),
		};
	}

	#[test]
	fn randexpr_compound_both_clauses() {
		let s =
			"generator name i32\n".to_string() +
			"state i32:random(i32:max() * i32:constant(2), " +
			"i32:min()+i32:constant(1)*i32:constant(2))";
		match generator::parse_LGeneratorList(s.as_str()) {
			Ok(_) => {},
			Err(e) => panic!("err: {:?}", e),
		};
	}

	#[test]
	fn randexpr_full() { // random() expression with compound sides
		let s =
			"generator name i32\n".to_string() +
			"state i32:random(i32:max() / i32:constant(2), " +
			"i32:max()-i32:constant(1))";
		match generator::parse_LGeneratorList(s.as_str()) {
			Ok(_) => {},
			Err(e) => panic!("err: {:?}", e),
		};
	}

	#[test]
	fn multiple_states() {
		let s = "generator name u64 state u64:min() state u64:max()";
		match generator::parse_LGeneratorList(s) {
			Ok(_) => {},
			Err(e) => panic!("err: {:?}", e),
		};
	}

	#[test]
	fn gen_interp_constant() {
		use variable::Generator;
		let s = "generator name u8 state u8:min()";
		let ugen = match generator::parse_LGeneratorList(s) {
			Ok(prs) => prs,
			Err(e) => panic!("parse error: {:?}", e),
		};
		let v = ugen[0].value();
		assert_eq!(v, "0");
	}

	#[test]
	fn gen_interp_compound() { // compound expression interpretation
		use variable::Generator;
		let mut s = "generator name u8 state u8:constant(4)+u8:constant(6)";
		let v = generator::parse_LGeneratorList(s).unwrap()[0].value();
		assert_eq!(v, "10");
		s = "generator name u8 state u8:constant(8)-u8:constant(6)";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap()[0].value(), "2");

		s = "generator name u8 state u8:constant(4)*u8:constant(5)";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap()[0].value(), "20");

		s = "generator name u8 state u8:constant(12)/u8:constant(2)";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap()[0].value(), "6");

		s = "generator name u8 state u8:constant(5) % u8:constant(2)";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap()[0].value(), "1");
	}

	#[test]
	fn names() {
		let mut s = "generator nm_1 i32 state i32:constant(6)\n".to_string();
		assert_eq!(generator::parse_LGeneratorList(s.as_str()).unwrap().len(), 1);

		s = "generator std:help i32 state i32:constant(6)\n".to_string();
		assert_eq!(generator::parse_LGeneratorList(s.as_str()).unwrap().len(), 1);
	}

	#[test]
	fn multi_gen() {
		let s =
			"generator name i32\n".to_string() +
			"state i32:random(i32:min(), i32:max())\n" +
			"generator second_name u32\n" +
			"state u32:min()" +
			"state u32:max()";
			assert_eq!(generator::parse_LGeneratorList(s.as_str()).unwrap().len(), 2);
	}

	#[test]
	fn gen_types() {
		let s = "generator name u8 state u8:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name u16 state u16:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name u32 state u32:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name u64 state u64:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name i8 state i8:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name i16 state i16:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name i32 state i32:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
		let s = "generator name i64 state i64:max()";
		assert_eq!(generator::parse_LGeneratorList(s).unwrap().len(), 1);
	}
}
