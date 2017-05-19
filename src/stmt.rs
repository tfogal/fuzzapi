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
			&Expression::FqnCall(ref fqn) => {
				fqn.retval.ty.clone()
			},
		}
	}
}

// A try that panic()s if it fails instead of returning an error.
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
			&Expression::FqnCall(ref fqn) => {
				let mut rv = String::new();
				tryp!(write!(&mut rv, "{}(", fqn.name));
				for (a, arg) in fqn.arguments.iter().enumerate() {
					tryp!(write!(&mut rv, "{}", arg.codegen()));
					if a != fqn.arguments.len()-1 {
						tryp!(write!(&mut rv, ", "));
					}
				}
				tryp!(write!(&mut rv, ")"));
				rv
			},
		}
	}
}

pub enum Statement {
	Expr(Expression),
	Assignment(Expression /* LHS */, Expression /* RHS */),
	Verify(Expression),
	/* should have 'if' and 'loop' etc. */
}

impl Code for Statement {
	fn codegen(&self) -> String {
		match self {
			&Statement::Expr(ref expr) => expr.codegen(),
			&Statement::Assignment(ref lhs, ref rhs) => {
				lhs.codegen() + " = " + rhs.codegen().as_str() + ";"
			},
			&Statement::Verify(ref expr) => {
				"assert(".to_string() + expr.codegen().as_str() + ");"
			},
		}
	}
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

	#[test]
	fn fqn_expr() {
		let null = variable::ScalarOp::Null;
		let r = variable::Source::free("rv", &Type::Builtin(Native::I32), null);
		let rv = ReturnType::new(&Type::Builtin(Native::I32), r);
		let fqn = Function::new("f", &rv, &vec![]);
		let fexpr = Expression::FqnCall(fqn);
		assert_eq!(fexpr.extype(), Type::Builtin(Native::I32));
		assert_eq!(fexpr.codegen(), "f()");
		drop(fexpr);

		// make sure it codegen's single argument...
		let fvar = variable::Source::free("Fv", &Type::Builtin(Native::I32), null);
		let arg = Argument::new(&Type::Builtin(Native::I32), fvar);
		let fqn = Expression::FqnCall(Function::new("g", &rv, &vec![arg]));
		assert_eq!(fqn.codegen(), "g(Fv)");
		drop(fqn);

		// .. and that it puts commas if there's an arglist...
		let va = variable::Source::free("Va", &Type::Builtin(Native::I32), null);
		let vb = variable::Source::free("Vb", &Type::Builtin(Native::I32), null);
		let a0 = Argument::new(&Type::Builtin(Native::I32), va);
		let a1 = Argument::new(&Type::Builtin(Native::I32), vb);
		let fqn = Expression::FqnCall(Function::new("h", &rv, &vec![a0, a1]));
		assert_eq!(fqn.codegen(), "h(Va, Vb)");
	}

	#[test]
	fn expr_statement() {
		use std::ops::Deref;

		let null = variable::ScalarOp::Null;
		let src = variable::Source::free("a", &Type::Builtin(Native::I32), null);
		let expr = Expression::Simple(null, src.deref().borrow().clone());
		let sstmt = Statement::Expr(expr);
		assert_eq!(sstmt.codegen(), "a");
		drop(sstmt); drop(src);

		let drf = variable::ScalarOp::Deref;
		let src = variable::Source::free("b", &Type::Builtin(Native::I32), drf);
		let expr = Expression::Simple(drf, src.deref().borrow().clone());
		let sstmt = Statement::Expr(expr);
		assert_eq!(sstmt.codegen(), "*b");
		drop(sstmt); drop(src);
	}

	#[test]
	fn assignment_stmt() {
		use std::ops::Deref;
		let null = variable::ScalarOp::Null;
		let dst = variable::Source::free("a", &Type::Builtin(Native::I32), null);
		let src = variable::Source::free("b", &Type::Builtin(Native::I32), null);
		let srcexp = Expression::Simple(null, src.deref().borrow().clone());
		let dstexp = Expression::Simple(null, dst.deref().borrow().clone());
		let sstmt = Statement::Assignment(dstexp, srcexp);
		assert_eq!(sstmt.codegen(), "a = b;");
	}

	#[test]
	fn verify_stmt() {
		use std::ops::Deref;
		let null = variable::ScalarOp::Null;
		let vara = variable::Source::free("a", &Type::Builtin(Native::I32), null);
		let expr = Expression::Simple(null, vara.deref().borrow().clone());
		let vstmt = Statement::Verify(expr);
		assert_eq!(vstmt.codegen(), "assert(a);");
	}
}
