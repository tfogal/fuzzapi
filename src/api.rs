#[derive(Debug, PartialEq)]
pub enum Native {
	U8, U16, U32, U64, Usize,
	I8, I16, I32, I64, Integer,
	Character,
	Pointer(Box<Native>),
}

pub type EnumValue = (String, i64);

pub enum DeclType {
	Builtin(Native),
	UDT(Vec<Declaration>),
	Enum(Vec<EnumValue>),
	UDTRef(String),
	EnumRef(String),
}

pub struct Declaration {
	pub name: String,
	pub ty: DeclType,
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
		let ref decl: api::Declaration = fuzz::parse_L_API(s).unwrap()[0];
		assert_eq!(decl.name, "entry".to_string());
		match decl.ty {
			api::DeclType::Builtin(_) => panic!("type should be UDT, is Builtin"),
			api::DeclType::Enum(_) => panic!("type should be UDT, is Enum"),
			api::DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			api::DeclType::UDTRef(_) => panic!("type should be UDT, is UDTRef"),
			api::DeclType::UDT(ref decllist) => {
				assert_eq!(decllist.len(), 0)
			},
		};
	}

	#[test]
	fn test_struct_pointer_char() {
		let s = "struct Ent { pointer char key; }";
		assert!(fuzz::parse_L_API(s).is_ok());
		assert_eq!(fuzz::parse_L_API(s).unwrap().len(), 1);
		let ref decl: api::Declaration = fuzz::parse_L_API(s).unwrap()[0];
		assert_eq!(decl.name, "Ent".to_string());
		match decl.ty {
			api::DeclType::Builtin(_) => panic!("type should be UDT, is Builtin"),
			api::DeclType::Enum(_) => panic!("type should be UDT, is Enum"),
			api::DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			api::DeclType::UDTRef(_) => panic!("type should be UDT, is UDTRef"),
			api::DeclType::UDT(ref decllist) => {
				assert_eq!(decllist.len(), 1);
				let ref key: api::Declaration = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					api::DeclType::UDT(_) => panic!("incorrect type UDT for 'key'"),
					api::DeclType::Enum(_) => panic!("incorrect type Enum for 'key'"),
					api::DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::UDTRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::Builtin(ref blt) => {
						let ch = api::Native::Character;
						assert_eq!(blt, &api::Native::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}
}
