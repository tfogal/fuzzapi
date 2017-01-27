use std::collections::btree_map::BTreeMap;

#[derive(Clone, PartialEq)]
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
