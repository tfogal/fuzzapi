use std::collections::btree_map::BTreeMap;

// A Type holds the basic immutable type information of the object.
#[derive(Clone, Debug, PartialEq)]
#[allow(dead_code)]
pub enum Type {
	U8, I8, U16, I16, U32, I32, U64, I64, F32, F64,
	Usize, Integer, Unsigned,
	Character,
	Void,
	Enum(String, BTreeMap<String, u32>), // name + set of values
	// Each Type in the vec is assumed to be a Field.
	UDT(String, Vec<Box<Type>>),
	Field(String, Box<Type>),
	Pointer(Box<Type>),
}

pub trait Name {
	fn name(&self) -> String;
}

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

impl Name for Type {
	fn name<'a>(&'a self) -> String {
		use std::fmt::Write;
		use typ::Type::*;
		let mut res = String::new();
		match self {
			&U8 => "uint8_t",
			&I8 => "int8_t",
			&U16 => "uint16_t", &I16 => "int16_t",
			&U32 => "uint32_t", &I32 => "int32_t",
			&U64 => "uint64_t", &I64 => "int64_t",
			&F32 => "float", &F64 => "double",
			&Usize => "size_t", &Integer => "int", &Unsigned => "unsigned",
			&Character => "char",
			&Void => "void",
			&Enum(ref enm, _) => {
				tryp!(write!(&mut res, "{}", enm));
				res.as_str().clone()
			},
			&Type::UDT(ref udt, _) => udt,
			&Type::Field(_, ref ty) => {
				tryp!(write!(&mut res, "{}", ty.name()));
				res.as_str().clone()
			},
			&Type::Pointer(ref t) => {
				tryp!(write!(&mut res, "{}*", t.name()));
				res.as_str().clone()
			},
		}.to_string()
	}
}
