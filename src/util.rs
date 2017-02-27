use typ::*;

pub fn type_from_str(typename: &str) -> Type {
	match typename {
		"I8" | "i8" => Type::I8,
		"I16" | "i16" => Type::I16,
		"I32" | "i32" => Type::I32,
		"I64" | "i64" => Type::I64,
		"U8" | "u8" => Type::U8,
		"U16" | "u16" => Type::U16,
		"U32" | "u32" => Type::U32,
		"U64" | "u64" => Type::U64,
		"Usize" => Type::Usize,
		"Integer" => Type::Integer,
		"Unsigned" => Type::Unsigned,
		"Character" => Type::Character,
		_ => panic!("unhandled '{}' type", typename),
	}
}
