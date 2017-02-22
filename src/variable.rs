// This holds information about a variable is used in an API.  Breifly:
//   Source: where the variable comes from / how it is generated
//   ScalarOp: transformation to apply to a variable to use in the context a
//             Source utilized in
//   Generator: holds the current/next state in the TypeClass list (tc.rs)
use std::cell::RefCell;
use std::rc::Rc;
use function::*;
use typ::*;
use tc::*;

#[derive(Debug)]
pub struct Source {
	name: String,
	pub generator: Box<Generator>,
	pub op: ScalarOp,
	// There is only ever one parent and/or function.  However we need to store
	// them in vectors because Rust is annoying and doesn't let us create an
	// "empty" RefCell.
	pub parent: Vec<Rc<RefCell<Source>>>,
	fqn: Vec<Rc<Function>>,
}
impl Source {
	// Construct a free variable of the given type that needs the given ScalarOp.
	pub fn free(nm: &str, ty: &Type, o: ScalarOp) -> Rc<RefCell<Source>> {
		Rc::new(RefCell::new(Source{
			name: nm.to_string(), generator: generator(ty), op: o,
			parent: Vec::new(),
			fqn: Vec::new(),
		}))
	}
	// Similar construction, but this takes an explicit generator for when the
	// default one for the type is inappropriate.
	pub fn free_gen(nm: &str, gen: Box<Generator>, o: ScalarOp) ->
		Rc<RefCell<Source>> {
		Rc::new(RefCell::new(Source{
			name: nm.to_string(), generator: gen, op: o,
			parent: Vec::new(),
			fqn: Vec::new(),
		}))
	}
	pub fn is_free(&self) -> bool {
		return self.name.len() != 0;
	}

	pub fn bound(parent: Rc<RefCell<Source>>, o: ScalarOp) -> Rc<RefCell<Source>> {
		Rc::new(RefCell::new(Source{
			name: "".to_string(), generator: Box::new(GenNothing{}), op: o,
			parent: vec![parent],
			fqn: Vec::new(),
		}))
	}
	pub fn is_bound(&self) -> bool {
		return self.parent.len() == 1;
	}

/*
	pub fn retval(fqn: Rc<Function>, oper: ScalarOp) -> Rc<RefCell<Source>> {
		Rc::new(RefCell::new(Source{
			name: "".to_string(), generator: Box::new(GenNothing{}), op: oper,
			parent: Vec::new(),
			fqn: vec![fqn],
		}))
	}
*/
	pub fn is_retval(&self) -> bool {
		return self.fqn.len() == 1;
	}
}

impl Name for Source {
	fn name(&self) -> String {
		if self.is_free() { return self.name.clone(); }
		if self.is_bound() { return self.parent[0].borrow().name(); }
		if self.is_retval() { return self.fqn[0].name.clone(); }
		println!("invalid source: {:?}", self);
		unreachable!();
	}
}

// A variable has a root type, but when used in functions it may need to be
// transformed in some way.  The classic example is a stack variable that needs
// address-of to be passed to a method that accepts it by pointer.
#[derive(Clone, Debug)]
pub enum ScalarOp {
	Null, // no transformation needed
	Deref, // dereference it once
	AddressOf, // apply the address-of operator
}
impl ToString for ScalarOp {
	fn to_string(&self) -> String {
		match self {
			&ScalarOp::Null => "".to_string(),
			&ScalarOp::Deref => "*".to_string(),
			&ScalarOp::AddressOf => "&".to_string(),
		}
	}
}

// A Generator holds TypeClass information and helps us iterate through the
// class of all values by knowing where we are in that sequence.
pub trait Generator {
	// Grabs the current state as an expression.
	fn value(&self) -> String;
	// Moves to the next state.  Does nothing if at the end state.
	fn next(&mut self);
	/// At the end state?
	fn done(&self) -> bool;
	fn n_state(&self) -> usize;
	// Sets the state back to 0.
	fn reset(&mut self);

	fn dbg(&self, &mut fmt::Formatter) -> fmt::Result;
}

/*
fn tjffmt(f: &mut fmt::Formatter, generator: &Box<Generator>) -> fmt::Result {
	if let Ok(gen) = generator.downcast::<GenEnum>() {
		write!(f, "{:?}", gen)
	} else {
		write!(f, "gener{{}}")
	}
}
*/

use std::fmt;
impl fmt::Debug for Box<Generator> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.dbg(f)
//		write!(f, "{:?}", self.dbg(f))
/*
		if let Ok(gen) = Box::downcast::<Generator>(self) {
			write!(f, "{:?}", gen)
		} else {
			write!(f, "gener{{}}")
		}
*/
	}
}

// There are special cases if you want to constrain the generator in some way.
// But if any value of that type will be fine, then you can just use this
// 'generator' method to get the most generic Generator for the given type.
pub fn generator(t: &Type) -> Box<Generator> {
	match t {
		&Type::Enum(_, _) => Box::new(GenEnum::create(t)),
		&Type::I32 => Box::new(GenI32::create(t)),
		&Type::Pointer(_) => Box::new(GenPointer::create(t)),
		&Type::Field(_, ref x) => generator(x),
		&Type::Usize => Box::new(GenUsize::create(t)),
		&Type::UDT(_, _) => Box::new(GenUDT::create(t)),
		&Type::Integer => {
			println!("WARNING: using I32 generator for integer!");
			Box::new(GenI32::create(t))
		}
		_ => panic!("unimplemented type {:?}", t), // for no valid reason
	}
}

//---------------------------------------------------------------------

// The generator attached to a Source will only be called if the source is a
// free variable.  Yet all Sources require a generator to be given.  So we use
// this generator on non-free Sources.
// It just panics if you call it, because you should never call it.
#[derive(Debug)]
pub struct GenNothing {}
// Maybe it's useful to have it pretend it's a 0-state thing that's always at
// the end?  Then we could do things like sum up all n_state()s in the tree of
// functions and have it make sense ...
impl Generator for GenNothing {
	fn value(&self) -> String { panic!("Null generator called"); }
	fn next(&mut self) { panic!("Null generator can't advance"); }
	fn done(&self) -> bool { return true; }
	fn n_state(&self) -> usize { 1 }
	fn reset(&mut self) {}
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "(none)")
	}
}

// Sometimes we have a "free" variable that is actually an opaque pointer and
// will be initialized by some API.  In that case we really can't generate
// values for it, so we use this special generator for it.
#[derive(Debug)]
pub struct GenOpaque {
	ty: Type,
}
impl GenOpaque {
	pub fn create(typ: &Type) -> Self {
		GenOpaque{ty: typ.clone()}
	}
}

impl Generator for GenOpaque {
	fn value(&self) -> String {
		let mut rv = String::new();
		use std::fmt::Write;
		write!(&mut rv, "/*({})*/{{}}", self.ty.name()).unwrap();
		return rv;
	}
	fn next(&mut self) {}
	fn done(&self) -> bool { return true; }
	fn n_state(&self) -> usize { 1 }
	fn reset(&mut self) {}
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "(opaque-none)")
	}
}

#[derive(Debug)]
pub struct GenEnum {
	cls: TC_Enum,
	idx: usize, // index into the list of values that this enum can take on
}

impl GenEnum {
	pub fn create(t: &Type) -> Self {
		GenEnum{cls: TC_Enum::new(t), idx: 0}
	}
}

impl Generator for GenEnum {
	fn value(&self) -> String {
		return self.cls.value(self.idx).to_string();
	}
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1;
		}
	}
	fn done(&self) -> bool {
		return self.idx >= self.cls.n()-1;
	}

	fn n_state(&self) -> usize {
		return self.cls.n();
	}

	fn reset(&mut self) { self.idx = 0; }
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "enum{{{} of {}}}", self.idx, self.cls.n())
	}
}

#[derive(Debug)]
pub struct GenI32 {
	cls: TC_I32,
	idx: usize,
}

impl GenI32 {
	pub fn create(_: &Type) -> Self {
		GenI32{ cls: TC_I32::new(), idx: 0 }
	}
}

impl Generator for GenI32 {
	fn value(&self) -> String {
		return self.cls.value(self.idx).to_string();
	}
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1
		}
	}
	fn done(&self) -> bool {
		return self.idx >= self.cls.n()-1;
	}

	fn n_state(&self) -> usize {
		return self.cls.n();
	}

	fn reset(&mut self) { self.idx = 0; }
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "i32{{{} of {}}}", self.idx, self.cls.n())
	}
}

#[derive(Debug)]
pub struct GenUsize {
	cls: TC_Usize,
	idx: usize,
}

impl GenUsize {
	pub fn create(_: &Type) -> Self {
		GenUsize{ cls: TC_Usize::new(), idx: 0 }
	}
}

impl Generator for GenUsize {
	fn value(&self) -> String {
		let mut rv = String::new();
		use std::fmt::Write;
		write!(&mut rv, "{}ull", self.cls.value(self.idx).to_string()).unwrap();
		return rv;
	}
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1
		}
	}
	fn done(&self) -> bool {
		return self.idx >= self.cls.n()-1;
	}

	fn n_state(&self) -> usize {
		return self.cls.n();
	}

	fn reset(&mut self) { self.idx = 0; }
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "usize{{{} of {}}}", self.idx, self.cls.n())
	}
}

#[derive(Debug)]
pub struct GenUDT {
	types: Vec<Type>,
	values: Vec<Box<Generator>>,
	idx: Vec<usize>,
}

impl GenUDT {
	pub fn create(t: &Type) -> Self {
		// UDT's 2nd tuple param is a Vec<Box<Type>>, but we want a Vec<Type>.
		let tys: Vec<Type> = match t {
			&Type::UDT(_, ref types) =>
				types.iter().map(|x| (**x).clone()).collect(),
			_ => panic!("{:?} type given to GenUDT!", t),
		};
		// create an appropriate value for every possible type.
		let mut val: Vec<Box<Generator>> = Vec::new();
		for x in tys.iter() {
			let v = generator(&x);
			val.push(v);
		}
		let nval: usize = val.len();
		assert_eq!(tys.len(), val.len());
		GenUDT{
			types: tys,
			values: val,
			// we need a vector of 0s the same size as 'values' or 'types'
			idx: (0..nval).map(|_| 0).collect(),
		}
	}
}

impl Generator for GenUDT {
	fn value(&self) -> String {
		use std::fmt::Write;
		let mut rv = String::new();

		write!(&mut rv, "{{\n").unwrap();

		for i in 0..self.values.len() {
			let nm = match self.types[i] {
				Type::Field(ref name, _) => name,
				ref x => panic!("GenUDT types are {:?}, not fields?", x),
			};
			write!(&mut rv, "\t\t.{} = {},\n", nm, self.values[i].value()).unwrap();
		}

		write!(&mut rv, "\t}}").unwrap();
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
		let nxt = match self.values.iter().rposition(|ref v| !v.done()) {
			None => /* already done.  just bail. */ { return; }
			Some(idx) => idx,
		};
		assert!(!self.values[nxt].done());
		self.values[nxt].next();
		for idx in nxt+1..self.values.len() {
			self.values[idx].reset();
		}
	}
	fn done(&self) -> bool {
		self.values.iter().all(|v| v.done())
	}

	fn reset(&mut self) {
		for v in 0..self.values.len() {
			self.values[v].reset();
		}
	}
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		try!(write!(f, "udt{{"));
		for (i, v) in self.values.iter().enumerate() {
			try!(write!(f, "f{}:", i));
			try!(v.dbg(f));
			if i != self.values.len()-1 {
				try!(write!(f, ", "));
			}
		}
		write!(f, "}}")
	}
}

#[derive(Debug)]
pub struct GenPointer {
	ty: Type,
	cls: TC_Pointer,
	idx: usize,
}

impl GenPointer {
	pub fn create(t: &Type) -> Self {
		match t {
			&Type::Pointer(_) => {},
			_ => panic!("asked to generate for non-pointer type {:?}", t),
		};
		GenPointer{ ty: t.clone(), cls: TC_Pointer::new(), idx: 0 }
	}
}

impl Generator for GenPointer {
	fn value(&self) -> String {
		let mut rv = String::new();
		use std::fmt::Write;
		write!(&mut rv, "({}){}ull", self.ty.name(),
		       self.cls.value(self.idx).to_string()).unwrap();
		return rv;
	}
	fn n_state(&self) -> usize { self.cls.n() }
	fn next(&mut self) {
		if self.idx < self.cls.n()-1 {
			self.idx = self.idx + 1
		}
	}
	fn done(&self) -> bool { return self.idx >= self.cls.n()-1; }
	fn reset(&mut self) { self.idx = 0; }
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "ptr{{{} of {}}}", self.idx, self.cls.n())
	}
}
