use function::*;
use typ::*;
use usergen::Opcode;
use variable;

// Code is anything we can generate code for.
trait Code {
	fn codegen(&self) -> String;
}

pub enum Expression {
	Simple(variable::ScalarOp, variable::Source),
	Compound(Box<Expression>, Opcode, Box<Expression>),
	FqnCall(Function),
}

impl Expression {
	pub fn extype(&self) -> Type {
		match self {
			&Expression::Simple(ref op, ref src) => {
				let tbasic = src.root();
				match op {
					&variable::ScalarOp::Null => tbasic.ty,
					&variable::ScalarOp::Deref => tbasic.ty.dereference(),
					&variable::ScalarOp::AddressOf => Type::Pointer(Box::new(tbasic.ty)),
				}
			}
			&Expression::Compound(ref lhs, ref op, ref rhs) => {
				let l = lhs.extype();
				let r = rhs.extype();
				op.result_type(l, r)
			},
			_ => unimplemented!(),
		}
	}
}

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

impl Code for Expression {
	fn codegen(&self) -> String {
		use std::fmt::Write;
		let mut rv = String::new();
		match self {
			&Expression::Simple(ref op, ref src) => {
				if src.is_free() {
					tryp!(write!(rv, "{}{}", op.to_string(), src.name()));
				} else if src.is_bound() {
					// correct?
					tryp!(write!(rv, "{}{}", op.to_string(), src.root().name()));
				} else if src.is_retval() {
					tryp!(write!(rv, "/*fixme, from ret!*/"));
				} else {
					unreachable!();
				}
				rv
			},
			_ => unimplemented!(),
		}
	}
}

pub enum Statement {
	Expr(Expression),
	Assignment(Expression /* LHS */, Expression /* RHS */),
	Verify(Expression),
	/* should have 'if' and 'loop' etc. */
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn simple() {
		let op = variable::ScalarOp::Null;
		let src = variable::Source::free("varname", &Type::Builtin(Native::I32), op);
		use std::ops::Deref;
		let expr = Expression::Simple(op, src.deref().borrow().clone());
		assert_eq!(expr.extype(), Type::Builtin(Native::I32));
	}
}
