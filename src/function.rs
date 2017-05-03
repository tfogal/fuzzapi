use std::cell::RefCell;
use std::rc::Rc;
use typ::*;
use variable::Source;

#[derive(Clone, Debug)]
pub struct Argument {
	pub ty: Type,
	pub src: Rc<RefCell<Source>>,
}
impl Argument {
	pub fn new(t: &Type, s: Rc<RefCell<Source>>) -> Self {
		Argument{ty: t.clone(), src: s}
	}
	pub fn source(&self) -> Source {
		return self.src.borrow().clone()
	}

	pub fn codegen(&self) -> String {
		self.src.borrow().name()
	}
}

#[derive(Clone, Debug)]
// Same as an Argument technically...
pub struct ReturnType {
	pub ty: Type,
	pub src: Rc<RefCell<Source>>,
}
impl ReturnType {
	pub fn new(t: &Type, s: Rc<RefCell<Source>>) -> Self {
		ReturnType{ty: t.clone(), src: s}
	}
}

#[derive(Clone, Debug)]
pub struct Function {
	pub retval: ReturnType,
	pub arguments: Vec<Argument>,
	pub name: String,
}
impl Function {
	pub fn new(nm: &str, rettype: &ReturnType, args: &Vec<Argument>) -> Self {
		Function{
			name: nm.to_string(),
			retval: rettype.clone(),
			arguments: args.clone(),
		}
	}
}
