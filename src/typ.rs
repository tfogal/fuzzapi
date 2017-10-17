use function;

// A Native type is a type that is builtin to the language.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Native {
	U8, U16, U32, U64, Unsigned, Usize,
	I8, I16, I32, I64, Integer,
	F32, F64,
	Boolean,
	Character,
	Void,
}

impl Native {
	// True if this type is "wider" than the given Native type.  Wider means that
	// it is always safe to assign a narrower-type to the wider-type, and almost
	// always unsafe to assign the other way.
	// These do not follow C rules, and are intended to be more restrictive and
	// force the user to cast.  Notable exceptions from C:
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

pub trait RTTI {
	fn type_name(&self) -> String;
}
impl RTTI for i8 { fn type_name(&self) -> String { "i8".to_string() } }
impl RTTI for i16 { fn type_name(&self) -> String { "i16".to_string() } }
impl RTTI for i32 { fn type_name(&self) -> String { "i32".to_string() } }
impl RTTI for i64 { fn type_name(&self) -> String { "i64".to_string() } }
impl RTTI for u8 { fn type_name(&self) -> String { "u8".to_string() } }
impl RTTI for u16 { fn type_name(&self) -> String { "u16".to_string() } }
impl RTTI for u32 { fn type_name(&self) -> String { "u32".to_string() } }
impl RTTI for u64 { fn type_name(&self) -> String { "u64".to_string() } }
impl RTTI for usize { fn type_name(&self) -> String { "usize".to_string() } }
impl RTTI for f32 { fn type_name(&self) -> String { "f32".to_string() } }
impl RTTI for f64 { fn type_name(&self) -> String { "f64".to_string() } }
impl RTTI for bool { fn type_name(&self) -> String { "bool".to_string() } }
impl RTTI for char { fn type_name(&self) -> String { "char".to_string() } }
impl RTTI for Native {
	fn type_name(&self) -> String {
		match *self {
			Native::U8 => "u8".to_string(), Native::U16 => "u16".to_string(),
			Native::U32 => "u32".to_string(), Native::U64 => "u64".to_string(),
			Native::I8 => "u8".to_string(), Native::I16 => "u16".to_string(),
			Native::I32 => "u32".to_string(), Native::I64 => "u64".to_string(),
			Native::Unsigned => "unsigned".to_string(),
			Native::Usize => "usize".to_string(),
			Native::Integer => "i32".to_string(),
			Native::F32 => "f32".to_string(), Native::F64 => "f64".to_string(),
			Native::Boolean => "bool".to_string(),
			Native::Character => "char".to_string(),
			Native::Void => "void".to_string(),
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
			// TODO: allow overloaded functions (match on args and/or rettype, too)?
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

impl RTTI for Type {
	fn type_name(&self) -> String {
		match self {
			&Type::Builtin(ref nat) => nat.type_name(),
			&Type::Pointer(ref base) => base.type_name() + "*",
			&Type::Struct(ref nm, ref flds) => {
				use std::fmt::Write;
				let mut rv = String::new();
				write!(&mut rv, "struct {} {{", nm).unwrap();
				for f in flds {
					//write!(&mut rv, "{}, ", (*f.1.deref()).type_name()).unwrap();
					write!(&mut rv, "{}, ", f.1.type_name()).unwrap();
				}
				write!(&mut rv, "}}").unwrap();
				rv
			},
			&Type::Enum(ref nm, _) => "enum ".to_string() + &nm,
			&Type::Function(ref fqn) => "func ".to_string() + &fqn.name,
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
			&Native::Boolean => "bool",
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
			&Type::Struct(ref udt, _) => "struct ".to_string() + &udt.clone(),
			&Type::Enum(ref enm, _) => enm.clone(),
			&Type::Function(ref fqn) => fqn.name.clone(),
		}
	}
}
