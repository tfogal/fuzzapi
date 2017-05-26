use std::cell::RefCell;
use std::rc::Rc;
use stmt;
use typ::*;
use variable::Source;

#[derive(Clone, Debug)]
pub struct Argument {
	pub ty: Type,
	pub src: Rc<RefCell<Source>>,
	pub expr: stmt::Expression,
}
impl Argument {
	pub fn new(t: &Type, s: Rc<RefCell<Source>>) -> Self {
		use variable;
		use std::ops::Deref;
		let exp = stmt::Expression::Simple(variable::ScalarOp::Null,
		                                   s.borrow().deref().clone());
		Argument{ty: t.clone(), src: s, expr: exp}
	}
	pub fn newexpr(t: &Type, expression: &stmt::Expression) -> Self {
		use variable;
		let nl = variable::ScalarOp::Null;
		let fake = variable::Source::free("blah", &Type::Builtin(Native::U8), nl);
		Argument{ty: t.clone(), src: fake, expr: expression.clone()}
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
