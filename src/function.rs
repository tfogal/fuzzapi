use typ::*;

pub type Parameter = Type;
pub type ReturnType = Type;

#[derive(Clone, Debug)]
pub struct Function {
	pub retval: ReturnType,
	pub parameters: Vec<Parameter>,
	pub name: String,
}
impl Function {
	pub fn new(nm: &str, rettype: &ReturnType, pm: &Vec<Parameter>) -> Self {
		Function{
			name: nm.to_string(),
			retval: rettype.clone(),
			parameters: pm.clone(),
		}
	}
}
