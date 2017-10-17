use std;
use std::io::{Error};
use api;
use expr::Expression;
use function::*;
use typ::*;
use opcode::{BinOp, UOp};

// Code is anything we can generate code for.
pub trait Code {
	fn codegen(&self, strm: &mut std::io::Write, program: &api::Program)
		-> Result<(),std::io::Error>;
}

// A try that panic()s if it fails instead of returning an error.
macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

#[derive(Clone, Debug)]
pub enum Statement {
	VariableDeclaration(String /* name */, Type),
	Expr(Expression),
	Assignment(Expression /* LHS */, Expression /* RHS */),
	Verify(Expression),
	Constraint(Expression),
	If(Expression, Box<Vec<Statement>> /* stmts if true. */),
	While(Expression, Box<Vec<Statement>> /* stmts if true. */),
	/* todo: 'loop' etc. */
}

impl Code for Statement {
	fn codegen(&self, strm: &mut std::io::Write, pgm: &api::Program)
		-> Result<(),Error> {
		match self {
			&Statement::VariableDeclaration(ref nm, _) => {
				let sym = pgm.symlookup(nm).unwrap();
				assert_eq!(sym.name, *nm);
				write!(strm, "{};", sym.generator.decl(nm))
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
			&Statement::Constraint(ref expr) => {
				// Constraints are sort of like verify expressions, but if the
				// constraint is not satisfied then we just can't test the program at
				// all.  As a bit of a hack, we still generate the whole program, we
				// just have the program exit early (successfully) if the constraint is
				// invalidated.
				try!(write!(strm, "if(!("));
				try!(expr.codegen(strm, pgm));
				try!(writeln!(strm, ")) {{"));
				try!(writeln!(strm, "\texit(EXIT_SUCCESS);"));
				write!(strm, "}}")
			},
			&Statement::If(ref expr, ref stlist) => {
				use std::ops::Deref;
				try!(write!(strm, "if("));
				try!(expr.codegen(strm, pgm));
				try!(writeln!(strm, ") {{"));
				for stmt in stlist.deref() {
					try!(write!(strm, "\t"));
					try!(stmt.codegen(strm, pgm));
				}
				writeln!(strm, "}}")
			},
			&Statement::While(ref expr, ref stlist) => {
				use std::ops::Deref;
				try!(write!(strm, "while("));
				try!(expr.codegen(strm, pgm));
				try!(writeln!(strm, ") {{"));
				for stmt in stlist.deref() {
					try!(write!(strm, "\t"));
					try!(stmt.codegen(strm, pgm));
				}
				writeln!(strm, "}}")
			},
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use variable;

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
			let dt = api::DeclType::Basic($vtype);
			let fvd = api::FreeVarDecl{name: $vname.to_string(),
			                           genname: "".to_string(), ty: dt};
			api::Stmt::Declaration(api::Declaration::Free(fvd))
		})
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
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("LHS", Type::Builtin(Native::I32)),
			vardecl!("RHS", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let l = pgm.symlookup("LHS").unwrap();
		let r = pgm.symlookup("RHS").unwrap();
		let null = UOp::None;
		let el = Box::new(Expression::Basic(null, l.clone()));
		let er = Box::new(Expression::Basic(null, r.clone()));
		compoundtest!(pgm, el, BinOp::Add, er, "LHS + RHS");
		compoundtest!(pgm, el, BinOp::Sub, er, "LHS - RHS");
		compoundtest!(pgm, el, BinOp::Mul, er, "LHS * RHS");
		compoundtest!(pgm, el, BinOp::Div, er, "LHS / RHS");
		compoundtest!(pgm, el, BinOp::Mod, er, "LHS % RHS");
		compoundtest!(pgm, el, BinOp::LAnd, er, "LHS && RHS");
		compoundtest!(pgm, el, BinOp::LOr, er, "LHS || RHS");
	}

	#[test]
	fn fqn_expr() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("rv", Type::Builtin(Native::I32)),
			vardecl!("Fv", Type::Builtin(Native::I32)),
			vardecl!("Va", Type::Builtin(Native::I32)),
			vardecl!("Vb", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let null = UOp::None;
		let va = Expression::Basic(null, pgm.symlookup("Va").unwrap().clone());
		let vb = Expression::Basic(null, pgm.symlookup("Vb").unwrap().clone());
		let fvar = Expression::Basic(null, pgm.symlookup("Fv").unwrap().clone());

		let rtype = Type::Builtin(Native::I32);
		let fqn = Function::new("f", &rtype, &vec![]);
		let fexpr = Expression::FqnCall(fqn, vec![]);

		assert_eq!(fexpr.extype(), Type::Builtin(Native::I32));
		cg_expect!(fexpr, "f()", pgm);
		drop(fexpr);

		// make sure it codegen's single argument...
		let argtype = fvar.extype();
		let fqn = Function::new("g", &rtype, &vec![argtype]);
		let fexpr = Expression::FqnCall(fqn, vec![fvar]);
		cg_expect!(fexpr, "g(Fv)", pgm);
		drop(fexpr);

		// .. and that it puts commas if there's an arglist...
		let a0 = va.extype();
		let a1 = vb.extype();
		let fqn = Function::new("h", &rtype, &vec![a0, a1]);
		let fexpr = Expression::FqnCall(fqn, vec![va, vb]);
		cg_expect!(fexpr, "h(Va, Vb)", pgm);
	}

	#[test]
	fn expr_statement() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
			vardecl!("b", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();

		let null = UOp::None;
		let src = pgm.symlookup("a").unwrap();
		let expr = Expression::Basic(null, src.clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "a;", pgm);
		drop(sstmt); drop(src);

		let drf = UOp::Deref;
		let src = pgm.symlookup("b").unwrap();
		let expr = Expression::Basic(drf, src.clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "*b;", pgm);
		drop(sstmt); drop(src);
	}

	#[test]
	fn assignment_stmt() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
			vardecl!("b", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let dst = pgm.symlookup("a").unwrap();
		let src = pgm.symlookup("b").unwrap();
		let null = UOp::None;
		let srcexp = Expression::Basic(null, src.clone());
		let dstexp = Expression::Basic(null, dst.clone());
		let sstmt = Statement::Assignment(dstexp, srcexp);
		cg_expect!(sstmt, "a = b;", pgm);
	}

	#[test]
	fn verify_stmt() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let vara = pgm.symlookup("a").unwrap();
		let null = UOp::None;
		let expr = Expression::Basic(null, vara.clone());
		let vstmt = Statement::Verify(expr);
		cg_expect!(vstmt, "assert(a);", pgm);
	}

	#[test]
	fn constraint_stmt() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let vara = pgm.symlookup("a").unwrap();
		let null = UOp::None;
		let lhs = Expression::Basic(null, vara.clone());
		let rhs = Expression::IConstant(0);
		let expr = Expression::Compound(Box::new(lhs), BinOp::Greater,
		                                Box::new(rhs));
		let cnstrnt = Statement::Constraint(expr);
		cg_expect!(cnstrnt, "if(!(a > 0)) {\n\texit(EXIT_SUCCESS);\n}", pgm);
	}

	#[test]
	fn field_expr() {
		let char_ptr = Type::Pointer(Box::new(Type::Builtin(Native::Character)));
		let void_ptr = Type::Pointer(Box::new(Type::Builtin(Native::Void)));
		let entry = Type::Struct("ENTRY".to_string(), vec![
			("key".to_string(), Box::new(char_ptr.clone())),
			("data".to_string(), Box::new(void_ptr.clone())),
		]);
		let foo = api::Symbol{name: "foo".to_string(),
		                      generator: variable::generator(&entry), typ: entry};
		let keyexpr = Expression::Field(foo.clone(), "key".to_string());
		let dataexpr = Expression::Field(foo, "data".to_string());
		assert_eq!(keyexpr.extype(), char_ptr);
		assert_eq!(dataexpr.extype(), void_ptr);
		let pgm = api::Program::new(&vec![], &vec![]);
		cg_expect!(keyexpr, "foo.key", pgm);
		cg_expect!(dataexpr, "foo.data", pgm);
	}

	#[test]
	fn if_statement() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let vara = pgm.symlookup("a").unwrap();
		let null = UOp::None;
		let simple = Expression::Basic(null, vara.clone());
		let ifst = Statement::If(simple, Box::new(vec![]));
		cg_expect!(ifst, "if(a) {\n}\n", pgm);
	}

	#[test]
	fn while_statement() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("foo", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let varfoo = pgm.symlookup("foo").unwrap();
		let null = UOp::None;
		let simple = Expression::Basic(null, varfoo.clone());
		let ifst = Statement::While(simple, Box::new(vec![]));
		cg_expect!(ifst, "while(foo) {\n}\n", pgm);
	}
}
