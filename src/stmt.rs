use std;
use std::io::{Error};
use api::*;
use function::*;
use typ::*;
use opcode::Opcode;
use variable;

// Code is anything we can generate code for.
pub trait Code {
	fn codegen(&self, strm: &mut std::io::Write, program: &Program)
		-> Result<(),std::io::Error>;
}

#[derive(Clone,Debug)]
pub enum Expression {
	Basic(variable::ScalarOp, Symbol),
	IConstant(i64),
	FConstant(f64),
	Compound(Box<Expression>, Opcode, Box<Expression>),
	// Since they return a value, we say function calls are expressions instead
	// of statements.  Then any expression, no matter how trivial, is a
	// Statement. This has the slightly undesirable property that "variable;" is
	// a representable statement, which is nonsense, but I suppose it mirrors C
	// so at least it's intuitive.
	FqnCall(Function),
	// Field expression is a field of a struct.
	Field(Symbol, String),
}

impl Expression {
	pub fn extype(&self) -> Type {
		match self {
			&Expression::Basic(ref op, ref src) => {
				match op {
					&variable::ScalarOp::Null => src.typ.clone(),
					&variable::ScalarOp::Deref => src.typ.dereference(),
					&variable::ScalarOp::AddressOf =>
						Type::Pointer(Box::new(src.typ.clone())),
				}
			},
			&Expression::IConstant(_) => {
				Type::Builtin(Native::I64)
			},
			&Expression::FConstant(_) => {
				Type::Builtin(Native::F64)
			},
			&Expression::Compound(ref lhs, ref op, ref rhs) => {
				let l = lhs.extype();
				let r = rhs.extype();
				op.result_type(l, r)
			},
			&Expression::FqnCall(ref fqn) => {
				fqn.retval.clone()
			},
			&Expression::Field(ref sym, ref fld) => {
				// "cast" to the Struct type from sym's type.
				let fields = match sym.typ {
					Type::Struct(_, ref flds) => flds,
					_ =>
						panic!("Field expr {} references {:?} type; must be a struct.",
						       fld, sym.typ),
				};
				use typ;
				let find_field_name = |f: &typ::Field| { f.0 == *fld };
				let idx = match fields.iter().position(find_field_name) {
					None => panic!("Struct '{:?}' has no field '{}'", sym.typ, fld),
					Some(i) => i,
				};
				use std::ops::Deref;
				fields[idx].1.deref().clone()
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
			&Expression::Basic(ref op, ref src) => {
				write!(strm, "{}{}", op.to_string(), src.name)
			},
			&Expression::IConstant(integer) => {
				write!(strm, "{}", integer)
			},
			&Expression::FConstant(fpval) => {
				write!(strm, "{}", fpval)
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
			&Expression::Field(ref sym, ref fld) => {
				write!(strm, "{}.{}", sym.name, fld)
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
	Constraint(Expression),
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
			}
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
		let expr = Expression::Basic(null, varname.clone());
		assert_eq!(expr.extype(), Type::Builtin(Native::I32));
		cg_expect!(expr, "varname", pgm);
		drop(expr);

		// make sure address of affects codegen.
		let addrof = variable::ScalarOp::AddressOf;
		let v2 = pgm.symlookup("var2").unwrap();
		let expr = Expression::Basic(addrof, v2.clone());
		cg_expect!(expr, "&var2", pgm);
		drop(expr);

		// make sure deref affects codegen.
		let addrof = variable::ScalarOp::Deref;
		let v3 = pgm.symlookup("var3").unwrap();
		let expr = Expression::Basic(addrof, v3.clone());
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
		let el = Box::new(Expression::Basic(null, l.clone()));
		let er = Box::new(Expression::Basic(null, r.clone()));
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
		let va = Expression::Basic(null, pgm.symlookup("Va").unwrap().clone());
		let vb = Expression::Basic(null, pgm.symlookup("Vb").unwrap().clone());
		let fvar = Expression::Basic(null, pgm.symlookup("Fv").unwrap().clone());

		let rtype = Type::Builtin(Native::I32);
		let fqn = Function::new("f", &rtype, &vec![]);
		let fexpr = Expression::FqnCall(fqn);

		assert_eq!(fexpr.extype(), Type::Builtin(Native::I32));
		cg_expect!(fexpr, "f()", pgm);
		drop(fexpr);

		// make sure it codegen's single argument...
		let arg = Argument::new(&fvar);
		let fqn = Expression::FqnCall(Function::new("g", &rtype, &vec![arg]));
		cg_expect!(fqn, "g(Fv)", pgm);
		drop(fqn);

		// .. and that it puts commas if there's an arglist...
		let a0 = Argument::new(&va);
		let a1 = Argument::new(&vb);
		let fqn = Expression::FqnCall(Function::new("h", &rtype, &vec![a0, a1]));
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
		let expr = Expression::Basic(null, src.clone());
		let sstmt = Statement::Expr(expr);
		cg_expect!(sstmt, "a;", pgm);
		drop(sstmt); drop(src);

		let drf = variable::ScalarOp::Deref;
		let src = pgm.symlookup("b").unwrap();
		let expr = Expression::Basic(drf, src.clone());
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
		let srcexp = Expression::Basic(null, src.clone());
		let dstexp = Expression::Basic(null, dst.clone());
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
		let expr = Expression::Basic(null, vara.clone());
		let vstmt = Statement::Verify(expr);
		cg_expect!(vstmt, "assert(a);", pgm);
	}

	#[test]
	fn constraint_stmt() {
		let mut pgm = Program::new(&vec![], &vec![
			vardecl!("a", Type::Builtin(Native::I32)),
		]);
		pgm.analyze().unwrap();
		let vara = pgm.symlookup("a").unwrap();
		let null = variable::ScalarOp::Null;
		let lhs = Expression::Basic(null, vara.clone());
		let rhs = Expression::IConstant(0);
		let expr = Expression::Compound(Box::new(lhs), Opcode::Greater,
		                                Box::new(rhs));
		let cnstrnt = Statement::Constraint(expr);
		cg_expect!(cnstrnt, "if(!(a > 0)) {\n\texit(EXIT_SUCCESS);\n}", pgm);
	}
}
