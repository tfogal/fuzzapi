use std::rc::Rc;
use typ::*;
use variable::Source;

// An argument has a type, a source for the variable we'll use there, and a
// scalar op that transforms the source into the appropriate type.
pub type Argument = (Type, Rc<Source>);

// a return type is really the same thing as an argument.
pub type ReturnType = (Type, Rc<Source>);

#[derive(Clone)]
pub struct Function {
	pub return_type: ReturnType,
	pub arguments: Vec<Argument>,
	pub name: String,
}
