use function;

// A Native type is a type that is builtin to the language.
#[derive(Clone, Debug, PartialEq)]
pub enum Native {
	U8, U16, U32, U64, Unsigned, Usize,
	I8, I16, I32, I64, Integer,
	F32, F64,
	Character,
	Void,
}

pub type EnumValue = (String, i64);
pub type Field = (String, Box<Type>);

// A Type holds the basic immutable type information of the object.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
	Builtin(Native),
	Pointer(Box<Type>),
	Struct(String, Vec<Field>),
	Enum(String, Vec<EnumValue>),
}

pub enum Decl {
	Ty(Type),
	Fqn(function::Function),
}

pub trait Name {
	fn name(&self) -> String;
}

macro_rules! tryp {
	($e:expr) => (match $e { Ok(f) => f, Err(g) => panic!("{}", g) })
}

impl Name for Native {
	fn name(&self) -> String {
		match self {
			&Native::U8 => "uint8_t",
			&Native::I8 => "int8_t",
			&Native::U16 => "uint16_t", &Native::I16 => "int16_t",
			&Native::U32 => "uint32_t", &Native::I32 => "int32_t",
			&Native::U64 => "uint64_t", &Native::I64 => "int64_t",
			&Native::F32 => "float", &Native::F64 => "double",
			&Native::Usize => "size_t", &Native::Integer => "int",
			&Native::Unsigned => "unsigned",
			&Native::Character => "char",
			&Native::Void => "void",
		}.to_string()
	}
}

impl Name for Type {
	fn name(&self) -> String {
		use std::fmt::Write;
		match self {
			&Type::Builtin(ref blt) => blt.name(),
			&Type::Pointer(ref t) => {
				let mut res = String::new();
				tryp!(write!(&mut res, "{}*", t.name()));
				res
			},
			&Type::Struct(ref udt, _) => udt.clone(),
			&Type::Enum(ref enm, _) => enm.clone(),
		}
	}
}
