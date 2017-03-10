use variable;

#[derive(Debug, PartialEq)]
pub enum Native {
	U8, U16, U32, U64, Usize,
	I8, I16, I32, I64, Integer,
	Character,
	Void,
	Pointer(Box<Native>),
}

pub type EnumValue = (String, i64);

#[derive(Debug)]
pub enum DeclType {
	Builtin(Native),
	Struct(Vec<UDTDecl>),
	Enum(Vec<EnumValue>),
	StructRef(String),
	EnumRef(String),
}

#[derive(Debug)]
pub struct UDTDecl {
	pub name: String,
	pub ty: DeclType,
}

pub struct FreeVarDecl {
	pub name: String,
	pub op: variable::ScalarOp,
	pub genname: String,
	pub ty: DeclType, // Struct(...) and Enum(...) are not valid, but *Refs are.
}

pub enum Declaration {
	Free(FreeVarDecl),
	UDT(UDTDecl),
}

#[cfg(test)]
mod test {
	use fuzz;
	use api;

	#[test]
	fn test_empty_struct() {
		let s = "struct entry { }";
		assert!(fuzz::parse_L_API(s).is_ok());
		assert_eq!(fuzz::parse_L_API(s).unwrap().len(), 1);
		let ref decl: api::UDTDecl = fuzz::parse_L_API(s).unwrap()[0];
		assert_eq!(decl.name, "entry".to_string());
		match decl.ty {
			api::DeclType::Builtin(_) => panic!("type should be UDT, is Builtin"),
			api::DeclType::Enum(_) => panic!("type should be UDT, is Enum"),
			api::DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			api::DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			api::DeclType::Struct(ref decllist) => {
				assert_eq!(decllist.len(), 0)
			},
		};
	}

	#[test]
	fn test_struct_pointer_char() {
		let s = "struct Ent { pointer char key; }";
		assert!(fuzz::parse_L_API(s).is_ok());
		assert_eq!(fuzz::parse_L_API(s).unwrap().len(), 1);
		let ref decl: api::UDTDecl = fuzz::parse_L_API(s).unwrap()[0];
		assert_eq!(decl.name, "Ent".to_string());
		match decl.ty {
			api::DeclType::Builtin(_) => panic!("type should be UDT, is Builtin"),
			api::DeclType::Enum(_) => panic!("type should be UDT, is Enum"),
			api::DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			api::DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			api::DeclType::Struct(ref decllist) => {
				assert_eq!(decllist.len(), 1);
				let ref key: api::UDTDecl = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					api::DeclType::Struct(_) => panic!("incorrect type UDT for 'key'"),
					api::DeclType::Enum(_) => panic!("incorrect type Enum for 'key'"),
					api::DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::Builtin(ref blt) => {
						let ch = api::Native::Character;
						assert_eq!(blt, &api::Native::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}

	#[test]
	fn test_struct_multiple_fields() {
		let s = "struct Entry {\n".to_string() +
			"pointer char key;\n" +
			"pointer void value;\n" +
		"}";
		assert!(fuzz::parse_L_API(s.as_str()).is_ok());
		assert_eq!(fuzz::parse_L_API(s.as_str()).unwrap().len(), 1);
		let ref decl: api::UDTDecl = fuzz::parse_L_API(s.as_str()).unwrap()[0];
		assert_eq!(decl.name, "Entry".to_string());
		match decl.ty {
			api::DeclType::Builtin(_) => panic!("type should be UDT, is Builtin"),
			api::DeclType::Enum(_) => panic!("type should be UDT, is Enum"),
			api::DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			api::DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			api::DeclType::Struct(ref decllist) => {
				assert_eq!(decllist.len(), 2);
				let ref key: api::UDTDecl = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					api::DeclType::Struct(_) => panic!("incorrect type UDT for 'key'"),
					api::DeclType::Enum(_) => panic!("incorrect type Enum for 'key'"),
					api::DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::Builtin(ref blt) => {
						let ch = api::Native::Character;
						assert_eq!(blt, &api::Native::Pointer(Box::new(ch)));
					}
				}
				let ref value: api::UDTDecl = decllist[1];
				assert_eq!(value.name, "value");
				match value.ty {
					api::DeclType::Struct(_) => panic!("incorrect type UDT for 'key'"),
					api::DeclType::Enum(_) => panic!("incorrect type Enum for 'key'"),
					api::DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::Builtin(ref blt) => {
						let ch = api::Native::Void;
						assert_eq!(blt, &api::Native::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}

	#[test]
	fn test_enum_single() {
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
	fn test_enum_multi() {
		let s = "enum Enumeration { FOO = 0 , BAR = 1 , BAZ = 42 , }";
		let decls = match fuzz::parse_L_API(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
	}

	#[test]
	fn test_fvar_one() {
		let s = "struct X { } var:free blah op:null gen:I32 i32";
		let decls = match fuzz::parse_L_API(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
	}
}
