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
	// Generate a C expression that could be used in initializing a value of this
	// variable.
	fn value(&self) -> String;
}

// A Value holds TypeClass information and helps us iterate through the
// class of all values by knowing where we are in the list.
// todo: rewrite this to not have a template parameter.  Instead of 'get'
// returning a type T, have it just return a string with the
// appropriately-typed value stringified.  The idea is we could write:
//   writeln!(value.type_name() + " v00 = " + value.get());
// or similar.  Importantly, this gets around the issue of recursive types
// (user-defined types).
pub trait Value<'a, T> {
	fn new(&'a Type) -> Self;
	// Grabs the current state.
	fn get(&self) -> T;
	// Moves to the next state.
	fn next(&mut self);
	fn n_state(&self) -> usize;
}

//---------------------------------------------------------------------

pub struct ValueEnum<'a> {
	ty: &'a Type,
	cls: TC_Enum,
	idx: usize, // index into the list of values that this enum can take on
}

impl<'a> Value<'a, i32> for ValueEnum<'a> {
	fn new(t: &'a Type) -> Self {
		ValueEnum{ty: t, cls: TC_Enum::new(t), idx: 0}
	}

	fn get(&self) -> i32 {
		return self.cls.value(self.idx);
	}
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1;
		}
	}

	fn n_state(&self) -> usize {
		return self.cls.n();
	}
}

pub struct ValueI32 {
	cls: TC_I32,
	idx: usize,
}

impl<'a> Value<'a, i32> for ValueI32 {
	fn new(_: &'a Type) -> Self {
		ValueI32{ cls: TC_I32::new(), idx: 0 }
	}

	fn get(&self) -> i32 {
		return self.cls.value(self.idx);
	}
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1
		}
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
	fn value(&self) -> String {
		use std::fmt::Write;
		let mut res = String::new();
		write!(&mut res, "{}", self.tested.get()).unwrap();
		return res;
	}
}

pub struct FreeI32<'a> {
	pub name: String,
	pub tested: ValueI32,
	pub dest: Use<'a>,
	pub ty: &'a Type,
}

impl<'a> Free for FreeI32<'a> {
	fn name(&self) -> String { return self.name.clone(); }
	fn value(&self) -> String {
		use std::fmt::Write;
		let mut res = String::new();
		write!(&mut res, "{}", self.tested.get()).unwrap();
		return res;
	}
}
