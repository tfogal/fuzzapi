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
// class of all values by knowing where we are in that sequence.
pub trait Value {
	// Grabs the current state as an expression.
	fn get(&self) -> String;
	// Moves to the next state.  Does nothing if at the end state.
	fn next(&mut self);
	fn n_state(&self) -> usize;
}

pub fn create(t: &Type) -> Box<Value> {
	match t {
		&Type::Enum(_, _) => Box::new(ValueEnum::create(t)),
		&Type::I32 => Box::new(ValueI32::create(t)),
		&Type::Pointer(_) => Box::new(ValuePointer::create(t)),
		_ => panic!("unimplemented type {:?}", t), // for no valid reason
	}
}

//---------------------------------------------------------------------

pub struct ValueEnum {
	cls: TC_Enum,
	idx: usize, // index into the list of values that this enum can take on
}

impl ValueEnum {
	pub fn create(t: &Type) -> Self {
		ValueEnum{cls: TC_Enum::new(t), idx: 0}
	}
}

impl Value for ValueEnum {
	fn get(&self) -> String {
		return self.cls.value(self.idx).to_string();
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

impl ValueI32 {
	pub fn create(_: &Type) -> Self {
		ValueI32{ cls: TC_I32::new(), idx: 0 }
	}
}

impl Value for ValueI32 {
	fn get(&self) -> String {
		return self.cls.value(self.idx).to_string();
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

pub struct ValueUDT {
	types: Vec<Type>,
	values: Vec<Box<Value>>,
	idx: Vec<usize>,
}

impl ValueUDT {
	pub fn create(t: &Type) -> Self {
		// UDT's 2nd tuple param is a Vec<Box<Type>>, but we want a Vec<Type>.
		let tys: Vec<Type> = match t {
			&Type::UDT(_, ref types) =>
				types.iter().map(|x| (**x).clone()).collect(),
			_ => panic!("{:?} type given to ValueUDT!", t),
		};
		// create an appropriate value for every possible type.
		let mut val: Vec<Box<Value>> = Vec::new();
		for x in tys.iter() {
			let v = create(&x);
			val.push(v);
		}
		let nval: usize = val.len();
		assert_eq!(tys.len(), val.len());
		ValueUDT{
			types: tys,
			values: val,
			// we need a vector of 0s the same size as 'values' or 'types'
			idx: (0..nval).map(|_| 0).collect(),
		}
	}
}

impl Value for ValueUDT {
	fn get(&self) -> String {
		use std::fmt::Write;
		let mut rv = String::new();
		write!(&mut rv, "{{\n").unwrap();

		for i in 0..self.values.len() {
			let nm = match self.types[i] {
				Type::Field(ref name, _) => name,
				_ => panic!("ValueUDT types are not fields?"),
			};
			write!(&mut rv, "\t.{} = {},\n", nm, self.values[i].get()).unwrap();
		}

		write!(&mut rv, "}}").unwrap();
		return rv;
	}

	// The number of states a UDT has is all possibilities of all fields.
	fn n_state(&self) -> usize {
		self.values.iter().fold(1, |acc, ref v| acc*v.n_state())
	}

	// We have an index for every field value.  It's sort-of an add-with-carry:
	// we try to add to the smallest integer, but when that overflows we jump to
	// the next field's index.
	// If we reset EVERY index, then we are actually at our end state and nothing
	// changes.
	fn next(&mut self) {
		for (i, v) in self.values.iter().enumerate() {
			if self.idx[i] < v.n_state()-1 {
				self.idx[i] = self.idx[i] + 1;
				return;
			}
			self.idx[i] = 0;
		}
		// if we got here, then we reset *everything*.  That means we were actually
		// done, and now we just accidentally reset all the indices to the default
		// state.  So here we re-reset them to the end state before returning.
		for (i, v) in self.values.iter().enumerate() {
			self.idx[i] = v.n_state()
		}
	}
}

pub struct ValuePointer {
	cls: TC_Pointer,
	idx: usize,
}

impl ValuePointer {
	pub fn create(_: &Type) -> Self {
		// doesn't seem to be a good way to assert that t is a &Type::Pointer...
		ValuePointer{ cls: TC_Pointer::new(), idx: 0 }
	}
}

impl Value for ValuePointer {
	fn get(&self) -> String { self.cls.value(self.idx).to_string() }
	fn n_state(&self) -> usize { self.cls.n() }
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1
		}
	}
}

//---------------------------------------------------------------------

pub struct FreeEnum<'a> {
	pub name: String,
	pub tested: ValueEnum,
	pub dest: Use<'a>,
	pub ty: &'a Type,
}

impl<'a> Free for FreeEnum<'a> {
	fn name(&self) -> String { return self.name.clone(); }
	fn value(&self) -> String {
		return self.tested.get();
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
		return self.tested.get();
	}
}

pub struct FreeUDT<'a> {
	pub name: String,
	pub tested: ValueUDT,
	pub dest: Use<'a>,
	pub ty: &'a Type,
}

impl<'a> Free for FreeUDT<'a> {
	fn name(&self) -> String { return self.name.clone(); }
	fn value(&self) -> String { self.tested.get() }
}
