use std::fmt;
use typ::{Native, Type};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum UOp {
	AddressOf, // apply the address-of operator
	Deref, // dereference it once
	Negate, // unary negation, i.e. the "-" in "-1.f"
	Not, // relational not, the "!" in "!foo"
	None,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
	Add, Sub, Mul, Div, Mod,
	LAnd, LOr,
	Greater, GreaterEqual, Less, LessEqual,
	NotEqual, Equal,
}

impl UOp {
	// Derives the type of an expression after applying this unary op.
	fn nat_result_type(&self, affects: Native) -> Native {
		match *self {
			UOp::AddressOf => unreachable!(), // retval not a Type, no Pointers avail
			UOp::Deref => unreachable!(), // ditto 'retval not a type'.
			UOp::Negate => match affects {
				Native::Boolean => Native::Boolean,
				Native::U8 | Native::U16 | Native::U32 | Native::U64 |
					Native::Unsigned | Native::Usize | Native::Character =>
						panic!("negating unary type!"),
				Native::I8 => Native::I8,
				Native::I16 => Native::I16,
				Native::I32 => Native::I32,
				Native::I64 => Native::I64,
				Native::Integer => Native::Integer,
				Native::F32 => Native::F32,
				Native::F64 => Native::F64,
				Native::Void => panic!("negating void type!"),
			},
			UOp::Not => Native::Boolean,
			UOp::None => affects,
		}
	}

	// Derives the type of an expression after applying this unary op.
	pub fn result_type(&self, affects: Type) -> Type {
		match *self {
			UOp::AddressOf =>	Type::Pointer(Box::new(affects)),
			UOp::Deref => match affects {
				Type::Pointer(to) => *(to.clone()),
				_ => unreachable!(),
			},
			UOp::Negate => match affects {
				Type::Builtin(nat) => Type::Builtin(self.nat_result_type(nat)),
				_ => unimplemented!(),
			},
			UOp::Not => Type::Builtin(Native::Boolean),
			UOp::None => affects,
		}
	}
}

impl BinOp {
	// Given a left hand side type and a right hand side type, derive the
	// appropriate type for the combined expression "lhs op rhs".
	// todo: return a proper error
	pub fn result_type(&self, lhs: Type, rhs: Type) -> Type {
		match *self {
			BinOp::LAnd => return Type::Builtin(Native::Boolean),
			BinOp::LOr => return Type::Builtin(Native::Boolean),
			BinOp::Greater => return Type::Builtin(Native::Boolean),
			BinOp::GreaterEqual => return Type::Builtin(Native::Boolean),
			BinOp::Less => return Type::Builtin(Native::Boolean),
			BinOp::LessEqual => return Type::Builtin(Native::Boolean),
			BinOp::NotEqual => return Type::Builtin(Native::Boolean),
			BinOp::Equal => return Type::Builtin(Native::Boolean),
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

impl fmt::Display for UOp {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match *self {
			UOp::AddressOf => write!(f, "&"),
			UOp::Deref => write!(f, "*"),
			UOp::Negate => write!(f, "-"),
			UOp::Not => write!(f, "!"),
			UOp::None => write!(f, ""),
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

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn uop_result_type() {
		assert_eq!(UOp::Negate.nat_result_type(Native::I8), Native::I8);
		assert_eq!(UOp::Negate.nat_result_type(Native::I16), Native::I16);
		assert_eq!(UOp::Negate.nat_result_type(Native::I32), Native::I32);
		assert_eq!(UOp::Negate.nat_result_type(Native::I64), Native::I64);
		assert_eq!(UOp::Negate.nat_result_type(Native::F32), Native::F32);
		assert_eq!(UOp::Negate.nat_result_type(Native::F64), Native::F64);
		assert_eq!(UOp::Negate.nat_result_type(Native::Integer), Native::Integer);
		assert_eq!(UOp::Negate.nat_result_type(Native::Boolean), Native::Boolean);
		assert_eq!(UOp::Not.nat_result_type(Native::Boolean), Native::Boolean);
		assert_eq!(UOp::Not.result_type(Type::Builtin(Native::Boolean)),
		           Type::Builtin(Native::Boolean));
	}

	macro_rules! result_nat_test {
		($left:expr, $bop:expr, $right:expr, $expected:expr) => (
			let l = Type::Builtin($left);
			let r = Type::Builtin($right);
			let exp = Type::Builtin($expected);
			assert_eq!($bop.result_type(l, r), exp);
		)
	}

	#[test]
	fn bop_result() {
		use typ::Native::*;
		result_nat_test!(Native::U8, BinOp::Add, Native::U8, Native::U8);
		result_nat_test!(Native::U8, BinOp::Sub, Native::U8, Native::U8);
		result_nat_test!(Native::U8, BinOp::Mul, Native::U8, Native::U8);
		result_nat_test!(Native::U8, BinOp::Div, Native::U8, Native::U8);
		result_nat_test!(Native::U8, BinOp::Mod, Native::U8, Native::U8);
		result_nat_test!(Native::U16, BinOp::Add, Native::U8, Native::U16);
		result_nat_test!(Native::U32, BinOp::Add, Native::U8, Native::U32);
		result_nat_test!(Native::U64, BinOp::Add, Native::U8, Native::U64);
		result_nat_test!(Native::U8, BinOp::Add, Native::U16, Native::U16);
		result_nat_test!(Native::U8, BinOp::Add, Native::U32, Native::U32);
		result_nat_test!(Native::U8, BinOp::Add, Native::U64, Native::U64);
		result_nat_test!(Native::F32, BinOp::Add, Native::F32, Native::F32);
		result_nat_test!(Native::F64, BinOp::Add, Native::F32, Native::F64);
		result_nat_test!(Native::F32, BinOp::Add, Native::F64, Native::F64);
		result_nat_test!(Boolean, BinOp::LAnd, Boolean, Boolean);
		result_nat_test!(Boolean, BinOp::LOr, Boolean, Boolean);
		result_nat_test!(Boolean, BinOp::NotEqual, Boolean, Boolean);
		result_nat_test!(Boolean, BinOp::Equal, Boolean, Boolean);
		result_nat_test!(U8, BinOp::Greater, U8, Boolean);
		result_nat_test!(U16, BinOp::GreaterEqual, U16, Boolean);
		result_nat_test!(U32, BinOp::Less, U32, Boolean);
		result_nat_test!(I32, BinOp::LessEqual, I32, Boolean);
	}
}
