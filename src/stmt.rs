use function::*;
use typ::*;
use usergen::Opcode;
use variable;

// Code is anything we can generate code for.
trait Code {
	fn codegen(&self) -> String;
}

#[derive(Clone,Debug)]
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
			&Expression::Compound(ref lhs, ref op, ref rhs) => {
				lhs.codegen() + op.to_string().as_str() + rhs.codegen().as_str()
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
	fn simple_expr() {
		let null = variable::ScalarOp::Null;
		let src = variable::Source::free("varname", &Type::Builtin(Native::I32),
		                                 null);
		use std::ops::Deref;
		let expr = Expression::Simple(null, src.deref().borrow().clone());
		assert_eq!(expr.extype(), Type::Builtin(Native::I32));
		assert_eq!(expr.codegen(), "varname");
		drop(expr);

		// make sure address of affects codegen.
		let addrof = variable::ScalarOp::AddressOf;
		let v2 = variable::Source::free("var2", &Type::Builtin(Native::I32), null);
		let expr = Expression::Simple(addrof, v2.deref().borrow().clone());
		assert_eq!(expr.codegen(), "&var2");
		drop(expr);

		// make sure deref affects codegen.
		let addrof = variable::ScalarOp::Deref;
		let ptr = Type::Pointer(Box::new(Type::Builtin(Native::I32)));
		let v3 = variable::Source::free("var3", &ptr, null);
		let expr = Expression::Simple(addrof, v3.deref().borrow().clone());
		assert_eq!(expr.codegen(), "*var3");
	}

	macro_rules! compoundtest {
		($left:expr, $op:expr, $right:expr, $gennedcode:expr) => (
			let cp_ = Expression::Compound($left.clone(), $op, $right.clone());
			assert_eq!(cp_.codegen(), $gennedcode);
			drop(cp_);
		)
	}
	#[test]
	fn compound_expr() {
		use std::ops::Deref;
		let null = variable::ScalarOp::Null;
		let l = variable::Source::free("LHS", &Type::Builtin(Native::I32), null);
		let r = variable::Source::free("RHS", &Type::Builtin(Native::I32), null);
		let el = Box::new(Expression::Simple(null, l.deref().borrow().clone()));
		let er = Box::new(Expression::Simple(null, r.deref().borrow().clone()));
		compoundtest!(el, Opcode::Add, er, "LHS+RHS");
		compoundtest!(el, Opcode::Sub, er, "LHS-RHS");
		compoundtest!(el, Opcode::Mul, er, "LHS*RHS");
		compoundtest!(el, Opcode::Div, er, "LHS/RHS");
		compoundtest!(el, Opcode::Mod, er, "LHS%RHS");
	}
}
