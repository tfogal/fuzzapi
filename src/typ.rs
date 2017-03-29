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
#[derive(Clone, Debug)]
pub enum Type {
	Builtin(Native),
	Pointer(Box<Type>),
	Struct(String, Vec<Field>),
	Enum(String, Vec<EnumValue>),
	Function(Box<function::Function>)
}

impl PartialEq for Type {
	fn eq(&self, other: &Type) -> bool {
		return match self {
			&Type::Builtin(ref x) => match other {
				&Type::Builtin(ref y) => x == y, _ => false
			},
			&Type::Pointer(ref x) => match other {
				&Type::Pointer(ref y) => x == y, _ => false
			},
			&Type::Struct(ref s, ref flds) => match other {
				&Type::Struct(ref t, ref oflds) => s==t && flds==oflds,
				_ => false,
			},
			&Type::Enum(ref s, ref vals) => match other {
				&Type::Enum(ref t, ref ovals) => s==t && vals==ovals,
				_ => false,
			},
			&Type::Function(ref fqn) => match other {
				&Type::Function(ref ofqn) => fqn.name == ofqn.name, _ => false,
			},
		}
	}
	fn ne(&self, other: &Type) -> bool {
		return !self.eq(other);
	}
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
			&Type::Function(ref fqn) => fqn.name.clone(),
		}
	}
}
