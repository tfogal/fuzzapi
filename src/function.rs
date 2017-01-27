use typ::*;

pub type Arguments = Vec<Type>;
pub type ReturnType = Type;

#[derive(Clone)]
pub struct Function {
	pub return_type: Type,
	pub arguments: Arguments,
	pub name: String,
}
