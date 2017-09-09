use std::fmt;
use typ::Type;

#[derive(Clone, Debug)]
pub enum Opcode {
	Add, Sub, Mul, Div, Mod,
	LAnd, LOr,
	Greater, Less,
	NotEqual, Equal,
	Not,
}
impl Opcode {
	// Given a left hand side type and a right hand side type, derive the
	// appropriate type for the combined expression "lhs op rhs".
	// todo: return a proper error
	pub fn result_type(&self, lhs: Type, rhs: Type) -> Type {
		// todo FIXME: we need a boolean type
		match self {
			&Opcode::LAnd => println!("WARNING: need bool type!"),
			&Opcode::LOr => println!("WARNING: need bool type!"),
			&Opcode::Greater => println!("WARNING: need bool type!"),
			&Opcode::Less => println!("WARNING: need bool type!"),
			&Opcode::NotEqual => println!("WARNING: need bool type!"),
			&Opcode::Equal => println!("WARNING: need bool type!"),
			_ => (),
		};
		let natlhs = match lhs {
			Type::Builtin(n) => n,
			_ => unimplemented!(), // only builtin types implemented, for now.
		};
		let natrhs = match rhs {
			Type::Builtin(n) => n,
			_ => unimplemented!(), // only builtin types implemented, for now.
		};
		if natlhs.wider(natrhs) || natlhs == natrhs {
			return Type::Builtin(natlhs);
		} else if natrhs.wider(natlhs) {
			return Type::Builtin(natrhs);
		}
		// only widening conversions are allowed.
		panic!("incompatible expression: {:?} {:?} {:?}", lhs, self, rhs);
	}
}
impl fmt::Display for Opcode {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			&Opcode::Add => write!(f, "+"),
			&Opcode::Sub => write!(f, "-"),
			&Opcode::Mul => write!(f, "*"),
			&Opcode::Div => write!(f, "/"),
			&Opcode::Mod => write!(f, "%"),
			&Opcode::LAnd => write!(f, "&&"),
			&Opcode::LOr => write!(f, "||"),
			&Opcode::Greater => write!(f, ">"),
			&Opcode::Less => write!(f, "<"),
			&Opcode::NotEqual => write!(f, "!="),
			&Opcode::Equal => write!(f, "=="),
			&Opcode::Not => write!(f, "!"),
		}
	}
}

pub enum UOp {
	Not,
}
#[derive(Debug)]
pub enum BinOp {
	Add, Sub, Mul, Div, Mod,
	LAnd, LOr,
	Greater, GreaterEqual, Less, LessEqual,
	NotEqual, Equal,
}

impl BinOp {
	// Given a left hand side type and a right hand side type, derive the
	// appropriate type for the combined expression "lhs op rhs".
	// todo: return a proper error
	pub fn result_type(&self, lhs: Type, rhs: Type) -> Type {
		// todo FIXME: we need a boolean type
		match *self {
			BinOp::LAnd => println!("WARNING: need bool type!"),
			BinOp::LOr => println!("WARNING: need bool type!"),
			BinOp::Greater => println!("WARNING: need bool type!"),
			BinOp::Less => println!("WARNING: need bool type!"),
			BinOp::NotEqual => println!("WARNING: need bool type!"),
			BinOp::Equal => println!("WARNING: need bool type!"),
			_ => (),
		};
		let natlhs = match lhs {
			Type::Builtin(n) => n,
			_ => unimplemented!(), // only builtin types implemented, for now.
		};
		let natrhs = match rhs {
			Type::Builtin(n) => n,
			_ => unimplemented!(), // only builtin types implemented, for now.
		};
		if natlhs.wider(natrhs) || natlhs == natrhs {
			return Type::Builtin(natlhs);
		} else if natrhs.wider(natlhs) {
			return Type::Builtin(natrhs);
		}
		// only widening conversions are allowed.
		panic!("incompatible expression: {:?} {:?} {:?}", lhs, self, rhs);
	}
}


pub enum Op {
	Unary(UOp),
	Binary(BinOp),
}

impl fmt::Display for UOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			UOp::Not => write!(f, "!"),
		}
	}
}

impl fmt::Display for BinOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			BinOp::Add => write!(f, "+"),
			BinOp::Sub => write!(f, "-"),
			BinOp::Mul => write!(f, "*"),
			BinOp::Div => write!(f, "/"),
			BinOp::Mod => write!(f, "%"),
			BinOp::LAnd => write!(f, "&&"),
			BinOp::LOr => write!(f, "||"),
			BinOp::Greater => write!(f, ">"),
			BinOp::GreaterEqual => write!(f, ">="),
			BinOp::Less => write!(f, "<"),
			BinOp::LessEqual => write!(f, "<="),
			BinOp::NotEqual => write!(f, "!="),
			BinOp::Equal => write!(f, "=="),
		}
	}
}

impl fmt::Display for Op {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			Op::Unary(ref uop) => write!(f, "{}", uop),
			Op::Binary(ref bop) => write!(f, "{}", bop),
		}
	}
}

mod test {
	use super::*;
	use typ::*;

	macro_rules! result_builtin_test {
		($left:expr, $bop:expr, $right:expr, $expected:expr) => (
			let l = Type::Builtin($left);
			let r = Type::Builtin($right);
			let exp = Type::Builtin($expected);
			assert_eq!($bop.result_type(l, r), exp);
		)
	}

	#[test]
	fn bop_result() {
		result_builtin_test!(Native::U8, BinOp::Add, Native::U8, Native::U8);
		result_builtin_test!(Native::U8, BinOp::Sub, Native::U8, Native::U8);
		result_builtin_test!(Native::U8, BinOp::Mul, Native::U8, Native::U8);
		result_builtin_test!(Native::U8, BinOp::Div, Native::U8, Native::U8);
		result_builtin_test!(Native::U8, BinOp::Mod, Native::U8, Native::U8);
		result_builtin_test!(Native::U16, BinOp::Add, Native::U8, Native::U16);
		result_builtin_test!(Native::U32, BinOp::Add, Native::U8, Native::U32);
		result_builtin_test!(Native::U64, BinOp::Add, Native::U8, Native::U64);
		result_builtin_test!(Native::U8, BinOp::Add, Native::U16, Native::U16);
		result_builtin_test!(Native::U8, BinOp::Add, Native::U32, Native::U32);
		result_builtin_test!(Native::U8, BinOp::Add, Native::U64, Native::U64);
		result_builtin_test!(Native::F32, BinOp::Add, Native::F32, Native::F32);
		result_builtin_test!(Native::F64, BinOp::Add, Native::F32, Native::F64);
		result_builtin_test!(Native::F32, BinOp::Add, Native::F64, Native::F64);
	}
}
