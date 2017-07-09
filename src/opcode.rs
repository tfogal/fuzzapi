use std::fmt;
use typ::Type;

#[derive(Clone, Debug)]
pub enum Opcode {
	Add, Sub, Mul, Div, Mod,
	LAnd, LOr,
	Greater, Less,
	NotEqual, Equal,
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
		}
	}
}
