use typ::*;

pub fn type_from_str(typename: &str) -> Type {
	Type::Builtin(match typename {
		"I8" | "i8" => Native::I8,
		"I16" | "i16" => Native::I16,
		"I32" | "i32" => Native::I32,
		"I64" | "i64" => Native::I64,
		"U8" | "u8" => Native::U8,
		"U16" | "u16" => Native::U16,
		"U32" | "u32" => Native::U32,
		"U64" | "u64" => Native::U64,
		"float" | "f32" => Native::F32,
		"double" | "f64" => Native::F64,
		"Usize" | "size_t" => Native::Usize,
		"Integer" => Native::Integer,
		"Unsigned" => Native::Unsigned,
		"Character" => Native::Character,
		_ => panic!("invalid typename {}", typename),
	})
}
