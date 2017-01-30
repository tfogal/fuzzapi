// This holds information about a variable is used in an API.  Breifly:
//   Source: where the variable comes from / how it is generated
//   Use: where the variable is consumed, i.e. which parameter to which fqn
//   Value: holds the current/next state in the TypeClass list (tc.rs)
//   Free: container for everything.  code gen.
use function::*;
use typ::*;
use tc::*;

// Identifies the source of a variable.  Variable values can be generated via
// return values, e.g. 'x = f()', or as paramater args, e.g. 'type x; f(&x);'.
pub enum Source<'a> {
	Free,
	Parameter(&'a Function, usize), // function + parameter index it comes from
	ReturnValue(&'a Function),
}

// Details where a value is used: which parameter of which function.
pub enum Use<'a> {
	Nil, // isn't used.
	Argument(&'a Function, usize), // function + parameter index it comes from
}

// Free is a container for all of the variable information.
pub trait Free {
	fn name(&self) -> String;
	// The number of states this has.
	fn n_state(&self) -> usize;
	// Apply the next state.
	fn next(&mut self);
}

// A Value holds TypeClass information and helps us iterate through the
// class of all values by knowing where we are in the list.
pub trait Value<'a, T> {
	fn new(&'a Type) -> Self;
	fn get(&mut self) -> T;
	fn n_state(&self) -> usize;
}

pub struct ValueEnum<'a> {
	ty: &'a Type,
	cls: TC_Enum,
	idx: usize, // index into the list of values that this enum can take on
}

impl<'a> Value<'a, i32> for ValueEnum<'a> {
	fn new(t: &'a Type) -> Self {
		ValueEnum{ty: t, cls: TC_Enum::new(t), idx: 0}
	}
	fn get(&mut self) -> i32 {
		let rv = self.idx;
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1;
		}
		return self.cls.value(rv);
	}
	fn n_state(&self) -> usize {
		return self.cls.n();
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
		return self.tested.cls.n();
	}
	fn next(&mut self) {
		/* unimplemented right now ... */
	}
}
