use std;
use std::io::Error;
use api::*;
use stmt;
use typ::*;

#[derive(Clone, Debug)]
pub struct Argument {
	pub expr: stmt::Expression,
}
impl Argument {
	pub fn new(expression: &stmt::Expression) -> Self {
		Argument{expr: expression.clone()}
	}

	pub fn codegen(&self, strm: &mut std::io::Write, pgm: &Program)
		-> Result<(),Error> {
		use stmt::Code;
		self.expr.codegen(strm, pgm)
	}
}

pub type ReturnType = Type;

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
