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
	SimpleSym(variable::ScalarOp, Symbol),
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
			&Expression::SimpleSym(ref op, ref src) => {
				match op {
					&variable::ScalarOp::Null => src.typ.clone(),
					&variable::ScalarOp::Deref => src.typ.dereference(),
					&variable::ScalarOp::AddressOf =>
						Type::Pointer(Box::new(src.typ.clone())),
				}
			},
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
			&Expression::SimpleSym(ref op, ref src) => {
				write!(strm, "{}{}", op.to_string(), src.name)
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
			&Statement::Expr(ref expr) => {
				try!(expr.codegen(strm, pgm));
				write!(strm, ";")
			},
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

	macro_rules! vardecl {
		($vname:expr, $vtype:expr) => ({
			let dt = DeclType::Basic($vtype);
			let fvd = FreeVarDecl{name: $vname.to_string(),
			                      genname: "".to_string(), ty: dt};
			Stmt::VarDecl(fvd)
		})
	}

	#[test]
	fn simple_expr() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("varname", Type::Builtin(Native::I32)),
			vardecl!("var2", Type::Builtin(Native::I32)),
			vardecl!("var3", Type::Pointer(Box::new(Type::Builtin(Native::I32)))),
		]);
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		pgm.set_generators(&g);
		pgm.analyze().unwrap();

		let null = variable::ScalarOp::Null;
		let varname = pgm.symlookup("varname").unwrap();
		let expr = Expression::SimpleSym(null, varname.clone());
		assert_eq!(expr.extype(), Type::Builtin(Native::I32));
		cg_expect!(expr, "varname", pgm);
		drop(expr);

		// make sure address of affects codegen.
		let addrof = variable::ScalarOp::AddressOf;
		let v2 = pgm.symlookup("var2").unwrap();
		let expr = Expression::SimpleSym(addrof, v2.clone());
		cg_expect!(expr, "&var2", pgm);
		drop(expr);

		// make sure deref affects codegen.
		let addrof = variable::ScalarOp::Deref;
		let v3 = pgm.symlookup("var3").unwrap();
		let expr = Expression::SimpleSym(addrof, v3.clone());
		cg_expect!(expr, "*var3", pgm);
	}

	macro_rules! compoundtest {
		($pgm:expr, $left:expr, $op:expr, $right:expr, $gennedcode:expr) => (
			let cp_ = Expression::Compound($left.clone(), $op, $right.clone());
			cg_expect!(cp_, $gennedcode, $pgm);
			drop(cp_);
		)
	}
	#[test]
	fn compound_expr() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("LHS", Type::Builtin(Native::I32)),
			vardecl!("RHS", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let l = pgm.symlookup("LHS").unwrap();
		let r = pgm.symlookup("RHS").unwrap();
		let null = variable::ScalarOp::Null;
		let el = Box::new(Expression::SimpleSym(null, l.clone()));
		let er = Box::new(Expression::SimpleSym(null, r.clone()));
		compoundtest!(pgm, el, Opcode::Add, er, "LHS + RHS");
		compoundtest!(pgm, el, Opcode::Sub, er, "LHS - RHS");
		compoundtest!(pgm, el, Opcode::Mul, er, "LHS * RHS");
		compoundtest!(pgm, el, Opcode::Div, er, "LHS / RHS");
		compoundtest!(pgm, el, Opcode::Mod, er, "LHS % RHS");
		compoundtest!(pgm, el, Opcode::LAnd, er, "LHS && RHS");
		compoundtest!(pgm, el, Opcode::LOr, er, "LHS || RHS");
	}

	#[test]
	fn fqn_expr() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("rv", Type::Builtin(Native::I32)),
			vardecl!("Fv", Type::Builtin(Native::I32)),
			vardecl!("Va", Type::Builtin(Native::I32)),
			vardecl!("Vb", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let null = variable::ScalarOp::Null;
		let va = Expression::SimpleSym(null, pgm.symlookup("Va").unwrap().clone());
		let vb = Expression::SimpleSym(null, pgm.symlookup("Vb").unwrap().clone());
		let fvar = Expression::SimpleSym(null, pgm.symlookup("Fv").unwrap().clone());

		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		// fixme: no Source!
		let r = variable::Source::free("rv", &Type::Builtin(Native::I32), "", &g);
		let rv = ReturnType::new(&Type::Builtin(Native::I32), r);
		let fqn = Function::new("f", &rv, &vec![]);
		let fexpr = Expression::FqnCall(fqn);

		assert_eq!(fexpr.extype(), Type::Builtin(Native::I32));
		cg_expect!(fexpr, "f()", pgm);
		drop(fexpr);

		// make sure it codegen's single argument...
		let arg = Argument::new(&Type::Builtin(Native::I32), &fvar);
		let fqn = Expression::FqnCall(Function::new("g", &rv, &vec![arg]));
		cg_expect!(fqn, "g(Fv)", pgm);
		drop(fqn);

		// .. and that it puts commas if there's an arglist...
		let a0 = Argument::new(&Type::Builtin(Native::I32), &va);
		let a1 = Argument::new(&Type::Builtin(Native::I32), &vb);
		let fqn = Expression::FqnCall(Function::new("h", &rv, &vec![a0, a1]));
		cg_expect!(fqn, "h(Va, Vb)", pgm);
	}

	#[test]
	fn expr_statement() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
			vardecl!("b", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();

		let null = variable::ScalarOp::Null;
		let src = pgm.symlookup("a").unwrap();
		let expr = Expression::SimpleSym(null, src.clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "a;", pgm);
		drop(sstmt); drop(src);

		let drf = variable::ScalarOp::Deref;
		let src = pgm.symlookup("b").unwrap();
		let expr = Expression::SimpleSym(drf, src.clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "*b;", pgm);
		drop(sstmt); drop(src);
	}

	#[test]
	fn assignment_stmt() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
			vardecl!("b", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let dst = pgm.symlookup("a").unwrap();
		let src = pgm.symlookup("b").unwrap();
		let null = variable::ScalarOp::Null;
		let srcexp = Expression::SimpleSym(null, src.clone());
		let dstexp = Expression::SimpleSym(null, dst.clone());
		let sstmt = Statement::Assignment(dstexp, srcexp);
		cg_expect!(sstmt, "a = b;", pgm);
	}

	#[test]
	fn verify_stmt() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let vara = pgm.symlookup("a").unwrap();
		let null = variable::ScalarOp::Null;
		let expr = Expression::SimpleSym(null, vara.clone());
		let vstmt = Statement::Verify(expr);
		cg_expect!(vstmt, "assert(a);", pgm);
	}
}
