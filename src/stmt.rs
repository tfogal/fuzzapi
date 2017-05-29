use std;
use std::io::{Error};
use api::*;
use function::*;
use typ::*;
use usergen::Opcode;
use variable;

// Code is anything we can generate code for.
pub trait Code {
	fn codegen(&self, strm: &mut std::io::Write, program: &Program)
		-> Result<(),std::io::Error>;
}

#[derive(Clone,Debug)]
pub enum Expression {
	Simple(variable::ScalarOp, variable::Source),
	Compound(Box<Expression>, Opcode, Box<Expression>),
	// Since they return a value, we say function calls are expressions instead
	// of statements.  Then any expression, no matter how trivial, is a
	// Statement. This has the slightly undesirable property that "variable;" is
	// a representable statement, which is nonsense, but I suppose it mirrors C
	// so at least it's intuitive.
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
	fn codegen(&self, strm: &mut std::io::Write, program: &Program)
		-> Result<(),Error> {
		match self {
			&Expression::Simple(ref op, ref src) => {
				if src.is_free() {
					try!(write!(strm, "{}{}", op.to_string(), src.name()));
				} else if src.is_bound() {
					// correct?
					try!(write!(strm, "{}{}", op.to_string(), src.root().name()));
				} else if src.is_retval() {
					try!(write!(strm, "/*fixme, from ret!*/"));
					unreachable!(); // right?
				} else {
					unreachable!();
				}
				Ok(())
			},
			&Expression::Compound(ref lhs, ref op, ref rhs) => {
				try!(lhs.codegen(strm, program));
				try!(write!(strm, " {} ", op.to_string()));
				rhs.codegen(strm, program)
			},
			&Expression::FqnCall(ref fqn) => {
				try!(write!(strm, "{}(", fqn.name));
				for (a, arg) in fqn.arguments.iter().enumerate() {
					try!(arg.codegen(strm, program));
					if a != fqn.arguments.len()-1 {
						try!(write!(strm, ", "));
					}
				}
				write!(strm, ")")
			},
		}
	}
}

impl Expression {
	// This creates variable declarations for everything that goes into this
	// expression.  Functions are ignored.
	pub fn decl(&self) -> String {
		let mut rv: String = String::new();
		use std::fmt::Write;
		match self {
			&Expression::Simple(_, ref src) => {
				let nm = src.name();
				write!(&mut rv, "{} {} = {};", src.ty.name(), nm,
				       src.generator.value()).unwrap();
			},
			&Expression::Compound(ref lhs, _, ref rhs) => {
				write!(&mut rv, "{}\n{}\n", lhs.decl(), rhs.decl()).unwrap();
			},
			&Expression::FqnCall(_) => {},
		}
		rv
	}
}

#[derive(Clone, Debug)]
pub enum Statement {
	VariableDeclaration(String /* name */, Type),
	Expr(Expression),
	Assignment(Expression /* LHS */, Expression /* RHS */),
	Verify(Expression),
	/* todo: 'if' and 'loop' etc. */
}

impl Code for Statement {
	fn codegen(&self, strm: &mut std::io::Write, pgm: &Program)
		-> Result<(),Error> {
		match self {
			&Statement::VariableDeclaration(ref nm, ref typ) => {
				let sym = pgm.symlookup(nm).unwrap();
				assert_eq!(sym.name, *nm);
				write!(strm, "{} {} = {};", typ.name(), nm, sym.generator.value())
			},
			&Statement::Expr(ref expr) => expr.codegen(strm, pgm),
			&Statement::Assignment(ref lhs, ref rhs) => {
				try!(lhs.codegen(strm, pgm));
				try!(write!(strm, " = "));
				try!(rhs.codegen(strm, pgm));
				write!(strm, ";")
			},
			&Statement::Verify(ref expr) => {
				try!(write!(strm, "assert("));
				try!(expr.codegen(strm, pgm));
				write!(strm, ");")
			},
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use std::ops::Deref;
	use variable::*;

	macro_rules! cg_expect {
		($left:expr, $expected:expr, $pgm:expr) => (
			let mut strm: Vec<u8> = Vec::new();
			match $left.codegen(&mut strm, &$pgm) {
				Err(e) => panic!(e),
				Ok(_) => (),
			};
			assert_eq!(String::from_utf8(strm).unwrap(), $expected);
		)
	}

	#[test]
	fn simple_expr() {
		let pgm = Program::new(&vec![], &vec![]);
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		let null = variable::ScalarOp::Null;
		let src = variable::Source::free("varname", &Type::Builtin(Native::I32),
		                                 "", &g);
		let expr = Expression::Simple(null, src.deref().borrow().clone());
		assert_eq!(expr.extype(), Type::Builtin(Native::I32));
		cg_expect!(expr, "varname", pgm);
		drop(expr);

		// make sure address of affects codegen.
		let addrof = variable::ScalarOp::AddressOf;
		let v2 = variable::Source::free("var2", &Type::Builtin(Native::I32), "",&g);
		let expr = Expression::Simple(addrof, v2.deref().borrow().clone());
		cg_expect!(expr, "&var2", pgm);
		drop(expr);

		// make sure deref affects codegen.
		let addrof = variable::ScalarOp::Deref;
		let ptr = Type::Pointer(Box::new(Type::Builtin(Native::I32)));
		let v3 = variable::Source::free("var3", &ptr, "", &g);
		let expr = Expression::Simple(addrof, v3.deref().borrow().clone());
		cg_expect!(expr, "*var3", pgm);
	}

	macro_rules! compoundtest {
		($left:expr, $op:expr, $right:expr, $gennedcode:expr) => (
			let pgm = Program::new(&vec![], &vec![]);
			let cp_ = Expression::Compound($left.clone(), $op, $right.clone());
			cg_expect!(cp_, $gennedcode, pgm);
			drop(cp_);
		)
	}
	#[test]
	fn compound_expr() {
		use std::ops::Deref;
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		let l = variable::Source::free("LHS", &Type::Builtin(Native::I32), "", &g);
		let r = variable::Source::free("RHS", &Type::Builtin(Native::I32), "", &g);
		let null = variable::ScalarOp::Null;
		let el = Box::new(Expression::Simple(null, l.deref().borrow().clone()));
		let er = Box::new(Expression::Simple(null, r.deref().borrow().clone()));
		compoundtest!(el, Opcode::Add, er, "LHS + RHS");
		compoundtest!(el, Opcode::Sub, er, "LHS - RHS");
		compoundtest!(el, Opcode::Mul, er, "LHS * RHS");
		compoundtest!(el, Opcode::Div, er, "LHS / RHS");
		compoundtest!(el, Opcode::Mod, er, "LHS % RHS");
		compoundtest!(el, Opcode::LAnd, er, "LHS && RHS");
		compoundtest!(el, Opcode::LOr, er, "LHS || RHS");
	}

	#[test]
	fn fqn_expr() {
		let pgm = Program::new(&vec![], &vec![]);
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		let r = variable::Source::free("rv", &Type::Builtin(Native::I32), "", &g);
		let rv = ReturnType::new(&Type::Builtin(Native::I32), r);
		let fqn = Function::new("f", &rv, &vec![]);
		let fexpr = Expression::FqnCall(fqn);
		assert_eq!(fexpr.extype(), Type::Builtin(Native::I32));
		cg_expect!(fexpr, "f()", pgm);
		drop(fexpr);

		// make sure it codegen's single argument...
		let fvar = variable::Source::free("Fv", &Type::Builtin(Native::I32), "",&g);
		let arg = Argument::new(&Type::Builtin(Native::I32), fvar);
		let fqn = Expression::FqnCall(Function::new("g", &rv, &vec![arg]));
		cg_expect!(fqn, "g(Fv)", pgm);
		drop(fqn);

		// .. and that it puts commas if there's an arglist...
		let va = variable::Source::free("Va", &Type::Builtin(Native::I32), "", &g);
		let vb = variable::Source::free("Vb", &Type::Builtin(Native::I32), "", &g);
		let a0 = Argument::new(&Type::Builtin(Native::I32), va);
		let a1 = Argument::new(&Type::Builtin(Native::I32), vb);
		let fqn = Expression::FqnCall(Function::new("h", &rv, &vec![a0, a1]));
		cg_expect!(fqn, "h(Va, Vb)", pgm);
	}

	#[test]
	fn expr_statement() {
		let pgm = Program::new(&vec![], &vec![]);
		use std::ops::Deref;
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];

		let null = variable::ScalarOp::Null;
		let src = variable::Source::free("a", &Type::Builtin(Native::I32), "", &g);
		let expr = Expression::Simple(null, src.deref().borrow().clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "a", pgm);
		drop(sstmt); drop(src);

		let drf = variable::ScalarOp::Deref;
		let src = variable::Source::free("b", &Type::Builtin(Native::I32), "", &g);
		let expr = Expression::Simple(drf, src.deref().borrow().clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "*b", pgm);
		drop(sstmt); drop(src);
	}

	#[test]
	fn assignment_stmt() {
		let pgm = Program::new(&vec![], &vec![]);
		use std::ops::Deref;
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		let dst = variable::Source::free("a", &Type::Builtin(Native::I32), "", &g);
		let src = variable::Source::free("b", &Type::Builtin(Native::I32), "", &g);
		let null = variable::ScalarOp::Null;
		let srcexp = Expression::Simple(null, src.deref().borrow().clone());
		let dstexp = Expression::Simple(null, dst.deref().borrow().clone());
		let sstmt = Statement::Assignment(dstexp, srcexp);
		cg_expect!(sstmt, "a = b;", pgm);
	}

	#[test]
	fn verify_stmt() {
		let pgm = Program::new(&vec![], &vec![]);
		use std::ops::Deref;
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		let vara = variable::Source::free("a", &Type::Builtin(Native::I32), "", &g);
		let null = variable::ScalarOp::Null;
		let expr = Expression::Simple(null, vara.deref().borrow().clone());
		let vstmt = Statement::Verify(expr);
		cg_expect!(vstmt, "assert(a);", pgm);
	}

	#[test]
	fn decl_simple() {
		use std::ops::Deref;
		let null = variable::ScalarOp::Null;
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		let vara = variable::Source::free("a", &Type::Builtin(Native::I32), "", &g);
		let expr = Expression::Simple(null, vara.deref().borrow().clone());
		/* The value here is very dependent on the initial state of the default
		 * generator for I32... */
		assert_eq!(expr.decl(), "int32_t a = -2147483648;");
	}
}
