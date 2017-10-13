use std;
use std::io::{Error};

use api;
use function::Function;
use opcode::{UOp, BinOp};
use stmt::Code;
use typ::{Native, Type};

#[derive(Clone,Debug)]
pub enum Expression {
	Basic(UOp, api::Symbol),
	IConstant(i64),
	FConstant(f64),
	Compound(Box<Expression>, BinOp, Box<Expression>),
	// Since they return a value, we say function calls are expressions instead
	// of statements.  Then any expression, no matter how trivial, is a
	// Statement. This has the slightly undesirable property that "variable;" is
	// a representable statement, which is nonsense, but I suppose it mirrors C
	// so at least it's intuitive.
	// This needs to have a vector of expressions for the arguments.
	FqnCall(Function),
	// Field expression is a field of a struct.
	Field(api::Symbol, String),
}

impl Expression {
	pub fn extype(&self) -> Type {
		match self {
			&Expression::Basic(ref op, ref src) => {
				match *op {
					UOp::AddressOf => Type::Pointer(Box::new(src.typ.clone())),
					UOp::Deref => src.typ.dereference(),
					UOp::Negate => {
						println!("FIXME negate of unsigned type should be signed.");
						src.typ.clone()
					},
					UOp::None => src.typ.clone(),
					UOp::Not => src.typ.clone(),
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

impl Code for Expression {
	fn codegen(&self, strm: &mut std::io::Write, program: &api::Program)
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

#[cfg(test)]
mod test {
	use super::*;
	use variable::{Generator, GenNothing};

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

	#[test]
	fn simple_expr() {
		let mut pgm = api::Program::new(&vec![], &vec![
			vardecl!("varname", Type::Builtin(Native::I32)),
			vardecl!("var2", Type::Builtin(Native::I32)),
			vardecl!("var3", Type::Pointer(Box::new(Type::Builtin(Native::I32)))),
		]);
		let g: Vec<Box<Generator>> = vec![Box::new(GenNothing{})];
		pgm.set_generators(&g);
		pgm.analyze().unwrap();

		let null = UOp::None;
		let varname = pgm.symlookup("varname").unwrap();
		let expr = Expression::Basic(null, varname.clone());
		assert_eq!(expr.extype(), Type::Builtin(Native::I32));
		cg_expect!(expr, "varname", pgm);
		drop(expr);

		// make sure address of affects codegen.
		let addrof = UOp::AddressOf;
		let v2 = pgm.symlookup("var2").unwrap();
		let expr = Expression::Basic(addrof, v2.clone());
		cg_expect!(expr, "&var2", pgm);
		drop(expr);

		// make sure deref affects codegen.
		let addrof = UOp::Deref;
		let v3 = pgm.symlookup("var3").unwrap();
		let expr = Expression::Basic(addrof, v3.clone());
		cg_expect!(expr, "*var3", pgm);
	}
}
