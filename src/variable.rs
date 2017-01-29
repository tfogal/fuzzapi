use function::*;
use typ::*;

pub enum Source<'a> {
	Free,
	Parameter(&'a Function, usize), // function + parameter index it comes from
	ReturnValue(&'a Function),
}

pub enum Use<'a> {
	Nil, // isn't used.
	Argument(&'a Function, usize), // function + parameter index it comes from
}

pub trait Free {
	fn name(&self) -> String;
	// The number of states this has.
	fn n_state(&self) -> usize;
	// Apply the next state.
	fn next(&mut self);
}

pub trait Value<'a, T> {
	fn new(&'a Type) -> Self;
	fn get(&mut self) -> T;
	fn n_state(&self) -> usize;
}

pub struct ValueEnum<'a> {
	ty: &'a Type,
}

impl<'a> Value<'a, i32> for ValueEnum<'a> {
	fn new(t: &'a Type) -> Self {
		ValueEnum{ty: t}
	}
	fn get(&mut self) -> i32 {
		return 42;
	}
	fn n_state(&self) -> usize {
		match self.ty {
			&Type::Enum(ref nm, ref values) => {
				return values.len()
			},
			_ => panic!("ValueEnum is not an enum?!"),
		}
	}
}

pub struct FreeEnum<'a> {
	pub name: String,
	pub tested: ValueEnum<'a>,
	pub dest: Use<'a>,
	pub ty: &'a Type,
}

impl<'a> Free for FreeEnum<'a> {
	fn name(&self) -> String { return self.name.clone(); }
	fn n_state(&self) -> usize {
		use tc::TypeClass;
		return self.n_state();
		//return self.tested.cls.n();
	}
	fn next(&mut self) {
		/* unimplemented right now ... */
	}
}
