use function;

// A Native type is a type that is builtin to the language.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Native {
	U8, U16, U32, U64, Unsigned, Usize,
	I8, I16, I32, I64, Integer,
	F32, F64,
	Character,
	Void,
}

impl Native {
	// True if this type is "wider" than the given Native type.  Wider means that
	// it is always safe to assign a narrower-type to the wider-type, and almost
	// always unsafe to assign the other way.
	// These do not follow C rules, and are intended to be more restrictive and
	// force the user to cast.  Notable exceptions:
	//   - Usize is assumed to be 32bit, even on 64bit hosts.
	//   - Integer is assumed to be 32bit
	//   - Characters are not integer types at all, and thus never "wider".
	//   - Floating point values are not integer types and cannot be wider or
	//     narrower than their integer counterparts.
	pub fn wider(&self, other: Native) -> bool {
		match self {
			&Native::U8 => false,
			&Native::U16 if other == Native::U8 => true,
			&Native::U32 if other == Native::U8 || other == Native::U16 => true,
			&Native::Usize if other == Native::U8 || other == Native::U16 => true,
			&Native::U64 if other == Native::U8 || other == Native::U16 ||
			                other == Native::U32 => true,
			&Native::I8 => false,
			&Native::I16 if other == Native::I8 => true,
			&Native::I32 if other == Native::I8 || other == Native::I16 => true,
			&Native::Integer if other == Native::I8 || other == Native::I16 => true,
			&Native::I64 if other == Native::I8 || other == Native::I16 ||
			                other == Native::I32 => true,
			&Native::F64 if other == Native::F32 => true,
			_ => false,
		}
	}
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

impl Type {
	// Remove one layer of pointer type.  Must be a Type::Pointer!
	pub fn dereference(&self) -> Type {
		use std::ops::Deref;
		match self {
			&Type::Pointer(ref inner) => inner.deref().clone(),
			_ => panic!("Can't deref a non-pointer type!"),
		}
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
