// This provides a type system and API for parsing code.
//
// The type system here mirrors typ::Type, but we use more strings here
// to reference types instead of fully instantiating them.  This makes
// sense for parsing, because it lets us parse without worrying too
// much about semantics, and thereby importantly means we do less error
// handling during parsing and more during subsequent semantic analysis.
use std::cell::RefCell;
use std::rc::Rc;
use function;
use typ::{EnumValue, Native, Type};
use variable;

#[derive(Debug)]
pub enum DeclType {
	Basic(Type),
	Struct(String, Vec<UDTDecl>),
	Enum(String, Vec<EnumValue>),
	StructRef(String),
	EnumRef(String),
}

impl DeclType {
	fn typename(&self) -> String {
		use typ::Name;
		match self {
			&DeclType::Basic(ref ty) => ty.name(),
			&DeclType::Struct(ref nm, _) => nm.clone(),
			&DeclType::Enum(ref nm, _) => nm.clone(),
			&DeclType::StructRef(ref to) => to.clone(),
			&DeclType::EnumRef(ref to) => to.clone(),
		}
	}
}

#[derive(Debug)]
pub struct UDTDecl {
	pub name: String,
	pub ty: DeclType,
}

#[derive(Debug)]
pub struct FreeVarDecl {
	pub name: String,
	pub op: variable::ScalarOp,
	pub genname: String,
	pub ty: DeclType, // Struct(...) and Enum(...) are not valid, but *Refs are.
}

#[derive(Debug)]
pub struct FuncDecl {
	pub name: String,
	pub retval: DeclType,
	pub arguments: Vec<DeclType>,
}

#[derive(Debug, PartialEq)]
pub struct FuncCall {
	pub name: String,
	pub retval: String, // the type name encoded as a string
	pub arguments: Vec<String>, // arguments encoded as a string.
}

#[derive(Debug)]
pub enum Declaration {
	Free(FreeVarDecl),
	Function(FuncDecl),
	UDT(DeclType), // Error if the DeclType is not a Struct || Enum!
}

// gives the type from the declaration.
// it needs to take the current type list as well, because this DeclType may
// reference other types, and it would need to produce boxes to those types.
fn type_from_decl(decl: &DeclType, types: &Vec<Type>) -> Type {
	match decl {
		&DeclType::Basic(ref ty) => ty.clone(),
		&DeclType::Struct(ref snm, ref flds) => {
			let mut flds_rv: Vec<(String, Box<Type>)> = Vec::new();
			for f in flds {
				match f.ty {
					DeclType::Basic(ref ty) =>
						flds_rv.push((f.name.clone(), Box::new(ty.clone()))),
					DeclType::Struct(_, _) => {
						// correct?
						let subtype = type_from_decl(&f.ty, types);
						flds_rv.push((f.name.clone(), Box::new(subtype)));
					},
					DeclType::Enum(_, _) => unreachable!(),
					DeclType::StructRef(ref nm) => {
						for t in types {
							match t {
								&Type::Struct(ref tgt, _) if *nm==*tgt => {
									flds_rv.push((f.name.clone(), Box::new(t.clone())));
									break;
								},
								_ => (),
							}
						}
					},
					DeclType::EnumRef(/*ref nm*/ _) => unimplemented!(),
				}
			}
			Type::Struct(snm.clone(), flds_rv)
		},
		&DeclType::Enum(ref enm, ref evalues) => {
			Type::Enum(enm.clone(), evalues.clone())
		},
		&DeclType::StructRef(ref nm) => {
			let mut rv: Type = Type::Builtin(Native::Void);
			for typex in types {
				match typex {
					&Type::Struct(ref strct, _) if strct == nm => rv = typex.clone(),
					&Type::Struct(ref strct, _) => {
						println!("struct '{}' is not a match for '{}'", strct, nm);
					}
					_ => {},
				};
			}
			/* Didn't find it?  Then bail, unknown type! */
			if rv == Type::Builtin(Native::Void) {
				panic!("Unknown struct '{}'!", nm);
			}
			rv
		}
		&DeclType::EnumRef(_) => unimplemented!(),
	}
}

fn func_from_decl(fqn: &FuncDecl, types: &Vec<Type>,
                  gen: &Vec<Box<variable::Generator>>) -> function::Function {
	let nullop = variable::ScalarOp::Null;
	let fauxsrc : Rc<RefCell<variable::Source>> =
		variable::Source::free_gen("???", "std:nothing", &gen, nullop);
	let retv = function::ReturnType::new(&type_from_decl(&fqn.retval, &types),
	                                     fauxsrc);
	let mut rv = function::Function{
		name: fqn.name.clone(),
		arguments: Vec::new(),
		retval: retv
	};
	for arg in fqn.arguments.iter() {
		let typedecl: Type = type_from_decl(&arg, &types);
		let src: Rc<RefCell<variable::Source>> =
			variable::Source::free_gen("???", "std:nothing", &gen, nullop);
		rv.arguments.push(function::Argument::new(&typedecl, src));
	}
	return rv;
}

// replaces the "Decl" types from this module with the typ::* counterparts,
// potentially panic'ing due to invalid semantics.
pub fn resolve_types(decls: &Vec<Declaration>,
                     generators: &mut Vec<Box<variable::Generator>>) ->
	(Vec<Type>, Vec<Rc<RefCell<variable::Source>>>) {
	assert!(decls.len() > 0);
	let mut drv: Vec<Type> = Vec::new();
	let mut vars: Vec<Rc<RefCell<variable::Source>>> = Vec::new();

	for decl in decls {
		match decl {
			&Declaration::Free(ref fr) => {
				let gname: String = if fr.genname == "opaque" {
					"std:opaque:".to_string() + &fr.ty.typename()
				} else {
					fr.genname.clone()
				};
				let fvar = variable::Source::free_gen(&fr.name, &gname, generators,
				                                      fr.op);
				vars.push(fvar);
			},
			&Declaration::Function(ref fqn) => {
				let func = func_from_decl(fqn, &drv, generators);
				drv.push(Type::Function(Box::new(func)));
			},
			&Declaration::UDT(ref udecl) => {
				let typedecl: Type = type_from_decl(&udecl, &drv);
				drv.push(typedecl.clone());
				match udecl {
					&DeclType::Struct(ref x, _) => {
						let opaque = variable::GenOpaque::create(&typedecl);
						generators.push(Box::new(opaque));
						x
					},
					&DeclType::Enum(ref x, _) => {
						let genenum = variable::GenEnum::create(&typedecl);
						generators.push(Box::new(genenum));
						x
					},
					_ => panic!("invalid DeclType {:?} for UDT", udecl),
				};
			},
		};
	}
	(drv, vec![])
}

#[cfg(test)]
mod test {
	use api;
	use fuzz;
	use typ::{Native, Type};
	use variable;

	#[test]
	fn empty_struct() {
		let s = "struct entry { }";
		assert!(fuzz::parse_L_API(s).is_ok());
		assert_eq!(fuzz::parse_L_API(s).unwrap().len(), 1);
		let ref decl: api::Declaration = fuzz::parse_L_API(s).unwrap()[0];
		let decl = match decl {
			&api::Declaration::UDT(ref udt) => udt,
			_ => panic!("invalid declaration parse {:?}", decl),
		};
		use api::DeclType;
		match decl {
			&DeclType::Basic(_) => panic!("type should be Struct, is Basic"),
			&DeclType::Enum(_, _) => panic!("type should be Struct, is Enum"),
			&DeclType::EnumRef(_) => panic!("type should be Struct, is EnumRef"),
			&DeclType::StructRef(_) => panic!("type should be Struct, is StructRef"),
			&DeclType::Struct(ref nm, ref decllist) => {
				assert_eq!(*nm, "entry".to_string());
				assert_eq!(decllist.len(), 0)
			},
		};
		let mut generators: Vec<Box<variable::Generator>> = Vec::new();
		let (decl, _) =
			api::resolve_types(&fuzz::parse_L_API(s).unwrap(), &mut generators);
		assert_eq!(decl.len(), 1);
	}

	#[test]
	fn struct_pointer_char() {
		let s = "struct Ent { pointer char key; }";
		assert!(fuzz::parse_L_API(s).is_ok());
		assert_eq!(fuzz::parse_L_API(s).unwrap().len(), 1);
		let ref decl: api::Declaration = fuzz::parse_L_API(s).unwrap()[0];
		let decl = match decl {
			&api::Declaration::UDT(ref udt) => udt,
			_ => panic!("invalid declaration parse {:?}", decl),
		};
		use api::DeclType;
		match decl {
			&DeclType::Basic(_) => panic!("type should be UDT, is Basic"),
			&DeclType::Enum(_, _) => panic!("type should be UDT, is Enum"),
			&DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			&DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			&DeclType::Struct(ref nm, ref decllist) => {
				assert_eq!(*nm, "Ent".to_string());
				assert_eq!(decllist.len(), 1);
				let ref key: api::UDTDecl = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					api::DeclType::Struct(_, _) => panic!("incorrect type UDT for 'key'"),
					api::DeclType::Enum(_, _) => panic!("incorrect type Enum for 'key'"),
					api::DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::Basic(ref blt) => {
						let ch = Type::Builtin(Native::Character);
						assert_eq!(blt, &Type::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}

	#[test]
	fn struct_multiple_fields() {
		let s = "struct Entry {\n".to_string() +
			"pointer char key;\n" +
			"pointer void value;\n" +
		"}";
		assert!(fuzz::parse_L_API(s.as_str()).is_ok());
		assert_eq!(fuzz::parse_L_API(s.as_str()).unwrap().len(), 1);
		let ref decl: api::Declaration = fuzz::parse_L_API(s.as_str()).unwrap()[0];
		let decl = match decl {
			&api::Declaration::UDT(ref udt) => udt,
			_ => panic!("invalid declaration parse {:?}", decl),
		};
		use api::DeclType;
		match decl {
			&DeclType::Basic(_) => panic!("type should be UDT, is Basic"),
			&DeclType::Enum(_, _) => panic!("type should be UDT, is Enum"),
			&DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			&DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			&DeclType::Struct(ref nm, ref decllist) => {
				assert_eq!(*nm, "Entry".to_string());
				assert_eq!(decllist.len(), 2);
				let ref key: api::UDTDecl = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					DeclType::Struct(_, _) => panic!("incorrect type UDT for 'key'"),
					DeclType::Enum(_, _) => panic!("incorrect type Enum for 'key'"),
					DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					DeclType::Basic(ref blt) => {
						let ch = Type::Builtin(Native::Character);
						assert_eq!(blt, &Type::Pointer(Box::new(ch)));
					}
				}
				let ref value: api::UDTDecl = decllist[1];
				assert_eq!(value.name, "value");
				match value.ty {
					DeclType::Struct(_, _) => panic!("incorrect type UDT for 'key'"),
					DeclType::Enum(_, _) => panic!("incorrect type Enum for 'key'"),
					DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					DeclType::Basic(ref blt) => {
						let ch = Type::Builtin(Native::Void);
						assert_eq!(blt, &Type::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}

	#[test]
	fn enum_single() {
		let s = "enum Enumeration { BLAH = 0 , }";
		match fuzz::parse_L_API(s) {
			Ok(_) => {},
			Err(e) => panic!("{:?}", e),
		};
		let t = "enum Enumeration { BLA = 0 , }";
		assert!(fuzz::parse_L_API(t).is_ok());
		assert_eq!(fuzz::parse_L_API(t).unwrap().len(), 1);
	}

	#[test]
	fn enum_multi() {
		let s = "enum Enumeration { FOO = 0 , BAR = 1 , BAZ = 42 , }";
		let decls = match fuzz::parse_L_API(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
	}

	#[test]
	fn struct_fvar_single() {
		let s = "struct X { } var:free blah op:null gen:I32 i32";
		let decls = match fuzz::parse_L_API(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 2);
	}

	#[test]
	fn parse_function_new() {
		let s = "function:new hcreate_r int {usize, pointer struct hsearch_data,}";
		let decls: Vec<api::Declaration> = match fuzz::parse_L_API(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
		let fqn = match decls[0] {
			api::Declaration::Function(ref f) => f,
			_ => panic!("non function type {:?}", decls[0]),
		};
		assert_eq!(fqn.name, "hcreate_r");
		match fqn.retval {
			api::DeclType::Basic(ref ty) => match ty {
				&Type::Builtin(ref t) => assert_eq!(*t, Native::Integer),
				_ => panic!("basic type, but {:?}, not integer", ty),
			},
			_ => panic!("retval should be a basic type, not {:?}", fqn.retval),
		};
		assert_eq!(fqn.arguments.len(), 2);
		match fqn.arguments[0] {
			api::DeclType::Basic(ref ty) => match ty {
				&Type::Builtin(ref t) => assert_eq!(*t, Native::Usize),
				_ => panic!("basic type, but {:?} not usize", ty),
			},
			_ => panic!("arg0 should be a basic type, not {:?}", fqn.arguments[0]),
		};
		let ptr: &Type = match fqn.arguments[1] {
			api::DeclType::Basic(ref ptr) => ptr,
			_ => panic!("invalid arg1: {:?}", fqn.arguments[1]),
		};
		let boxptr = match ptr {
			&Type::Pointer(ref b) => b,
			_ => panic!("invalid ptr type {:?}", ptr),
		};
		use std::ops::Deref;
		match boxptr.deref() {
			&Type::Struct(ref nm, _) => assert_eq!(nm, "hsearch_data"),
			_ => panic!("invalid box ptr {:?}", boxptr),
		};
	}

	#[test]
	fn parse_two_function_decls() {
		let s = "function:new hcreate_r int {".to_string() +
			"usize, pointer struct hsearch_data," +
		"}" +
		"function:new hsearch_r int {" +
			"int, int, pointer pointer int, pointer struct hsearch_data," +
		"}";
		let decls: Vec<api::Declaration> = match fuzz::parse_L_API(s.as_str()) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 2);
		let fqn = match decls[0] {
			api::Declaration::Function(ref f) => f,
			_ => panic!("non function type {:?}", decls[0]),
		};
		assert_eq!(fqn.name, "hcreate_r");
	}

	#[test]
	fn func_from_decl() {
		let declint = api::DeclType::Basic(Type::Builtin(Native::Integer));
		let declszt = api::DeclType::Basic(Type::Builtin(Native::Usize));
		let fd = api::FuncDecl{
			name: "hcreate".to_string(),
			retval: declint,
			arguments: vec![declszt]
		};
		let generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let typelist: Vec<Type> = Vec::new();
		api::func_from_decl(&fd, &typelist, &generators);
	}

	#[test]
	fn type_resolution() {
		let s = "struct Entry {\n".to_string() +
			"pointer char key;\n" +
			"pointer void value;\n" +
		"}\n" +
		"var:free tbl op:addressof gen:opaque udt:Entry";
		let decls: Vec<api::Declaration> = match fuzz::parse_L_API(s.as_str()) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let (types, srcs) = api::resolve_types(&decls, &mut generators);
		assert_eq!(types.len(), 1);
		assert_eq!(srcs.len(), 0);
	}

	#[test]
	fn opaque_struct_in_function() {
		let s = "struct hsearch_data {}\n".to_string() +
		"var:free tbl op:addressof gen:opaque udt:hsearch_data\n" +
		"function:new hcreate_r int {" +
			"usize, pointer struct hsearch_data,\n" +
		"}\n";
		let decls: Vec<api::Declaration> = match fuzz::parse_L_API(s.as_str()) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 3);
		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let (types, _) = api::resolve_types(&decls, &mut generators);
		assert_eq!(generators.len(), 2);
		match types[0] {
			Type::Struct(ref nm, ref fields) => {
				assert_eq!(nm, "hsearch_data");
				assert_eq!(fields.len(), 0);
			}
			_ => panic!("first type ({:?}) should be struct hsearch_data", types[0]),
		};
		// should assert that the hcreate_r's 2nd arg == types[0].
	}

	#[test]
	fn enum_resolves_with_generator() {
		let s = "enum ACTION { ENTER=0, FIND=1, }\n";
		let decls: Vec<api::Declaration> = match fuzz::parse_L_API(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let (types, _) = api::resolve_types(&decls, &mut generators);
		assert_eq!(generators.len(), 2);
		assert_eq!(types.len(), 1);
		match types[0] {
			Type::Enum(ref enm, _) => assert_eq!(enm, "ACTION"),
			_ => (),
		};
	}
}
