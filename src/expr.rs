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
	FConstant(f64),
	IConstant(i64),
	UConstant(u64),
	Compound(Box<Expression>, BinOp, Box<Expression>),
	// Since they return a value, we say function calls are expressions instead
	// of statements.  Then any expression, no matter how trivial, is a
	// Statement. This has the slightly undesirable property that "variable;" is
	// a representable statement, which is nonsense, but I suppose it mirrors C
	// so at least it's intuitive.
	FqnCall(Function, Vec<Expression>),
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
			&Expression::FConstant(_) => Type::Builtin(Native::F64),
			&Expression::IConstant(_) => Type::Builtin(Native::I64),
			&Expression::UConstant(_) => Type::Builtin(Native::U64),
			&Expression::Compound(ref lhs, ref op, ref rhs) => {
				let l = lhs.extype();
				let r = rhs.extype();
				op.result_type(l, r)
			},
			&Expression::FqnCall(ref fqn, _) => {
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
			&Expression::FConstant(fpval) => {
				write!(strm, "{:.16}", fpval)
			},
			&Expression::IConstant(integer) => {
				write!(strm, "{}", integer)
			},
			&Expression::UConstant(uinteger) => {
				write!(strm, "{}", uinteger)
			},
			&Expression::Compound(ref lhs, ref op, ref rhs) => {
				try!(lhs.codegen(strm, program));
				try!(write!(strm, " {} ", op.to_string()));
				rhs.codegen(strm, program)
			},
			&Expression::FqnCall(ref fqn, ref args) => {
				assert_eq!(fqn.parameters.len(), args.len());
				try!(write!(strm, "{}(", fqn.name));
				for (a, arg) in args.iter().enumerate() {
					try!(arg.codegen(strm, program));
					if a != fqn.parameters.len()-1 {
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

	#[test]
	fn constexpr() {
		let exprf = Expression::FConstant(1.0 as f64);
		let expri = Expression::IConstant(1 as i64);
		let expru = Expression::UConstant(1 as u64);
		assert_eq!(exprf.extype(), Type::Builtin(Native::F64));
		assert_eq!(expri.extype(), Type::Builtin(Native::I64));
		assert_eq!(expru.extype(), Type::Builtin(Native::U64));
		let mut pgm = api::Program::new(&vec![], &vec![]);
		pgm.analyze().unwrap();
		cg_expect!(exprf, "1.0000000000000000", pgm);
		cg_expect!(expri, "1", pgm);
		cg_expect!(expru, "1", pgm);
	}
}
