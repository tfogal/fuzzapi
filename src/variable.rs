// This holds information about a variable is used in the API.  Briefly:
//   ScalarOp: transformation to apply to a variable to use in the context a
//             Source utilized in
//   Generator: holds the current/next state in the TypeClass list (tc.rs)
use std::fmt::{Display, Write};
use std::ops::Deref;
extern crate rand;
use rand::distributions::{IndependentSample, Range};
use expr::Expression;
use typ::*;
use tc::*;

// A Generator holds TypeClass information and helps us iterate through the
// class of all values by knowing where we are in that sequence.
pub trait Generator {
	// The name of this generator, as a user might invoke it.
	fn name(&self) -> String;

	// Given a variable name, return a statement that declares a variable using
	// the initial state of this generator.
	fn decl(&self, varname: &str) -> String;

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

	// Workaround because we can't clone() a trait, or a Box<> of one.
	fn clone(&self) -> Box<Generator>;
}

use std::fmt;
impl fmt::Debug for Box<Generator> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		self.dbg(f)
	}
}

pub fn natgenerator(t: &Native) -> Box<Generator> {
	match t {
		&Native::I32 => Box::new(GenI32::create(&Type::Builtin(t.clone()))),
		&Native::Usize => Box::new(GenUsize::create(&Type::Builtin(t.clone()))),
		&Native::Integer => {
			println!("WARNING: using I32 generator for integer!");
			Box::new(GenI32::create(&Type::Builtin(t.clone())))
		}
		_ => panic!("unimplemented native type {:?}", t),
	}
}
// There are special cases if you want to constrain the generator in some way.
// But if any value of that type will be fine, then you can just use this
// 'generator' method to get the most generic Generator for the given type.
pub fn generator(t: &Type) -> Box<Generator> {
	match t {
		&Type::Builtin(ref n) => natgenerator(n),
		&Type::Enum(_, _) => Box::new(GenEnum::create(t)),
		// Pointers to characters are interpreted to mean CStrings.
		&Type::Pointer(ref ty)
			if match **ty { // guard on type being a builtin ...
				Type::Builtin(ref n) if match n { // ... and that builtin being char
					&Native::Character => true, _ => false,
				} => true, _ => false,
			} => Box::new(GenCString::create(t)),
		// Pointers to anything else are just generic pointers...
		&Type::Pointer(_) => Box::new(GenPointer::create(t)),
		&Type::Struct(_, ref flds) => {
			if flds.len() == 0 {
				Box::new(GenOpaque::create(t))
			} else {
				Box::new(GenStruct::create(t))
			}
		},
		&Type::Function(_) => unimplemented!(),
	}
}

pub fn generator_single(t: &Type) -> Box<Generator> {
	match *t {
		Type::Function(_) => unimplemented!(),
		Type::Builtin(ref nat) => match *nat {
			Native::Boolean => Box::new(SingleGen::<bool>::create()),
			Native::U8 => Box::new(SingleGen::<u8>::create()),
			Native::U16 => Box::new(SingleGen::<u16>::create()),
			Native::U32 => Box::new(SingleGen::<u32>::create()),
			Native::U64 => Box::new(SingleGen::<u64>::create()),
			Native::I8 => Box::new(SingleGen::<i8>::create()),
			Native::I16 => Box::new(SingleGen::<i16>::create()),
			Native::I32 => Box::new(SingleGen::<i32>::create()),
			Native::I64 => Box::new(SingleGen::<i64>::create()),
			Native::Unsigned => Box::new(SingleGen::<u32>::create()),
			Native::Usize => Box::new(SingleGen::<usize>::create()),
			Native::Integer => Box::new(SingleGen::<i32>::create()),
			Native::F32 => Box::new(SingleGen::<f32>::create()),
			Native::F64 => Box::new(SingleGen::<f64>::create()),
			Native::Character => Box::new(SingleGen::<char>::create()),
			Native::Void => unreachable!(),
		},
		_ => unreachable!(),
	}
}

struct SingleGen<T> {
	#[allow(dead_code)]
	unused: T, // if it's missing, Rust complains that 'T' is not used.
}
impl<T: Default> Default for SingleGen<T> {
	fn default() -> Self { SingleGen::<T>{unused: Default::default()} }
}
impl<T: Clone + Default + Display> SingleGen<T> {
	pub fn create() -> Self { SingleGen::<T>::default() }
}

impl<T: 'static + Clone + Default + RTTI + ToString> Generator for
SingleGen<T> {
	fn name(&self) -> String {
		// todo fixme: get the type 'T' somehow (RTTI?) as a string in the name.
		let foo: T = Default::default();
		"std:single:".to_string() + &foo.type_name()
	}
	fn decl(&self, varname: &str) -> String {
		let foo: T = Default::default();
		let mut rv = String::new();
		write!(&mut rv, "{} {} = {}", stringify!(T), varname,
		       foo.to_string()).unwrap();
		return rv;
	}
	fn value(&self) -> String {
		let foo: T = Default::default();
		foo.to_string()
	}
	fn next(&mut self) {}
	fn done(&self) -> bool { true }
	fn n_state(&self) -> usize { 1 }
	fn reset(&mut self) {}
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "singlegen{{{} of {}}}", 1, 1)
	}
	fn clone(&self) -> Box<Generator> {
		Box::new(SingleGen::<T>::default())
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
	fn name(&self) -> String { "std:nothing".to_string() }
	#[allow(unused_variables)]
	fn decl(&self, ignored: &str) -> String { unimplemented!(); }
	fn value(&self) -> String { panic!("Null generator called"); }
	fn next(&mut self) { panic!("Null generator can't advance"); }
	fn done(&self) -> bool { return true; }
	fn n_state(&self) -> usize { 1 }
	fn reset(&mut self) {}
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "(none)")
	}
	fn clone(&self) -> Box<Generator> { Box::new(GenNothing{}) }
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
	fn name(&self) -> String {
		"std:opaque:".to_string() + self.ty.name().as_str()
	}
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		write!(&mut rv, "{} {} = /*({})*/{{}}", self.ty.name(), varname,
		       self.ty.name()).unwrap();
		return rv;
	}
	fn value(&self) -> String {
		let mut rv = String::new();
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
	fn clone(&self) -> Box<Generator> { Box::new(GenOpaque{ty: self.ty.clone()}) }
}

#[derive(Debug)]
pub struct GenEnum {
	name: String,
	cls: TC_Enum,
	idx: usize, // index into the list of values that this enum can take on
	typename: String
}

impl GenEnum {
	pub fn create(t: &Type) -> Self {
		GenEnum{name: "std:enum:".to_string() + t.name().as_str(),
		        cls: TC_Enum::new(t), idx: 0, typename: t.name()}
	}
}

impl Generator for GenEnum {
	fn name(&self) -> String { self.name.clone() }
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		write!(&mut rv, "{} {} = {}", self.typename, varname,
		       self.value()).unwrap();
		return rv;
	}
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
	fn clone(&self) -> Box<Generator> {
		Box::new(GenEnum{name: self.name.clone(), cls: self.cls.clone(),
		                 idx: self.idx, typename: self.typename.clone()})
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
	fn name(&self) -> String { "std:I32orig".to_string() }
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		write!(&mut rv, "int32_t {} = {}", varname, self.value()).unwrap();
		return rv;
	}
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
	fn clone(&self) -> Box<Generator> {
		Box::new(GenI32{cls: self.cls.clone(), idx: self.idx})
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
	fn name(&self) -> String { "std:usize".to_string() }
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		write!(&mut rv, "size_t {} = {}", varname, self.value()).unwrap();
		return rv;
	}
	fn value(&self) -> String {
		let mut rv = String::new();
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
	fn clone(&self) -> Box<Generator> {
		Box::new(GenUsize{cls: self.cls.clone(), idx: self.idx})
	}
}

#[derive(Debug)]
pub struct GenStruct {
	fields: Vec<Field>,
	values: Vec<Box<Generator>>,
	idx: Vec<usize>,
	typename: String,
}

impl GenStruct {
	pub fn create(t: &Type) -> Self {
		// Struct's 2nd tuple param is a Vec<(String, Box<Type>)>, but we want a
		// Vec<Type>.
		let tys: Vec<Type> = match t {
			&Type::Struct(_, ref flds) =>
				flds.iter().map(|x| (*(*x).1).clone()).collect(),
			_ => panic!("{:?} type given to GenStruct!", t),
		};
		// create an appropriate value for every possible type.
		let mut val: Vec<Box<Generator>> = Vec::new();
		for x in tys.iter() {
			let v = generator(&x);
			val.push(v);
		}
		let nval: usize = val.len();
		assert_eq!(tys.len(), val.len());
		let fld = match t {
			&Type::Struct(_, ref flds) => flds.clone(),
			_ => panic!("invalid struct type"),
		};
		assert_eq!(fld.len(), val.len());
		GenStruct{
			fields: fld,
			values: val,
			// we need a vector of 0s the same size as 'values' or 'fields'
			idx: (0..nval).map(|_| 0).collect(),
			typename: match *t { Type::Struct(ref nm, _) => nm.clone(),
			                     _ => panic!("not a struct.") },
		}
	}

	fn clone_values(&self) -> Vec<Box<Generator>> {
		let mut rv: Vec<Box<Generator>> = Vec::new();
		for v in self.values.iter() {
			rv.push((*v).clone());
		}
		return rv;
	}
}

impl Generator for GenStruct {
	fn name(&self) -> String { "std:Struct".to_string() }
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		write!(&mut rv, "struct {} {} = {}", self.typename, varname,
		       self.value()).unwrap();
		return rv;
	}
	fn value(&self) -> String {
		let mut rv = String::new();

		write!(&mut rv, "{{\n").unwrap();

		for i in 0..self.values.len() {
			let ref nm: String = self.fields[i].0;
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
	fn clone(&self) -> Box<Generator> {
		Box::new(GenStruct{fields: self.fields.clone(),
		                   values: self.clone_values(), idx: self.idx.clone(),
		                   typename: self.typename.clone()})
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
	fn name(&self) -> String { "std:pointer".to_string() }
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		// note that we don't need a '*' here because it is part of the type.
		write!(&mut rv, "{} {} = {}", self.ty.name(), varname,
		       self.value()).unwrap();
		return rv;
	}
	fn value(&self) -> String {
		let mut rv = String::new();
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
	fn clone(&self) -> Box<Generator> {
		Box::new(GenPointer{ty: self.ty.clone(), cls: self.cls.clone(),
		                    idx: self.idx})
	}
}

// Generate an arbitrary CString.
// NULL, i.e. not a string.
// 0 length strings
// 1 character strings of a 'normal' character
// 1 character strings of a 'special' character
// N character strings of 'normal' characters
// N character strings of 'special' characters
// N character strings mixing normal+special characters
// very long strings
pub struct GenCString {
	idx: usize,
	printable: TC_Char_Printable,
	control: TC_Char_Special,
}

// Manually implement debug instead of derive()ing it.  This works around rand's
// "Range" not implementing debug.  Of course, we don't actually care to print
// out the state of random ranges anyway.
impl ::std::fmt::Debug for GenCString {
	fn fmt(&self, f: &mut ::std::fmt::Formatter) -> fmt::Result {
		self.dbg(f)
	}
}

impl GenCString {
	pub fn create(t: &Type) -> Self {
		let x = Type::Pointer(Box::new(Type::Builtin(Native::Character)));
		assert!(*t == x);
		GenCString{idx: 0, printable: TC_Char_Printable::new(),
		           control: TC_Char_Special::new() }
	}

	// Generate a 'normal' character that is valid in strings.  This means:
	//   No ?: groups of ??anything are lame C trigraphs,
	//   No ": as it might terminate the string early.
	//   No \: it could escape the next character, which might be the end, ".
	fn normal(&self) -> char {
		let mut x: char = self.printable.value(0);
		let disallowed: [char;3] = ['"', '?', '\\'];
		while disallowed.iter().any(|y| x == *y) {
			x = self.printable.value(0);
		}
		return x as char;
	}

	// Generate a 'special' character that is valid in strings.
	fn special(&self) -> char {
		let mut x: char = self.control.value(0);
		let disallowed = [0,7,8,9,10,11,12,13, 27];
		while disallowed.iter().any(|y| x as u8 == *y) {
			x = self.control.value(0);
		}
		return x as char;
	}
}

impl Generator for GenCString {
	fn name(&self) -> String { "std:cstring".to_string() }
	fn decl(&self, varname: &str) -> String {
		let mut rv = String::new();
		write!(rv, "char* {} = {}", varname, self.value()).unwrap();
		return rv;
	}
	fn value(&self) -> String {
		// special case null, so that we can wrap all other cases in "".
		if self.idx == 0 {
			return "NULL".to_string();
		}

		let mut rv = String::new();
		write!(&mut rv, "\"").unwrap();
		assert!(self.idx < 8);
		match self.idx {
			0 => panic!("we already handled this case, above."),
			1 => {}, // just ""
			2 => { // a single normal character:
				write!(&mut rv, "{}", self.normal()).unwrap();
			},
			3 => { // a single special character:
				write!(&mut rv, "{}", self.special()).unwrap();
			},
			4 => { // a collection of N normal characters:
				let mut rng: rand::ThreadRng = rand::thread_rng();
				let length = Range::new(3,128).ind_sample(&mut rng);
				for _ in 0..length {
					write!(&mut rv, "{}", self.normal()).unwrap();
				}
			},
			5 => { // a collection of N special characters:
				let mut rng: rand::ThreadRng = rand::thread_rng();
				let length = Range::new(3,128).ind_sample(&mut rng);
				for _ in 0..length {
					write!(&mut rv, "{}", self.special()).unwrap();
				}
			},
			6 => { // a collection of N characters with normal + special mixed.
				let mut rng: rand::ThreadRng = rand::thread_rng();
				let length = Range::new(3,128).ind_sample(&mut rng);
				for _ in 0..length {
					if Range::new(0, 1).ind_sample(&mut rng) == 0 {
						write!(&mut rv, "{}", self.normal()).unwrap();
					} else {
						write!(&mut rv, "{}", self.special()).unwrap();
					}
				}
			},
			7 => { // absurdly long strings.
				let mut rng: rand::ThreadRng = rand::thread_rng();
				let length = Range::new(512,32768).ind_sample(&mut rng);
				for _ in 0..length {
					write!(&mut rv, "{}", self.normal()).unwrap();
				}
			},
			_ => panic!("unhandled case {}", self.idx),
		};
		write!(&mut rv, "\"").unwrap();
		return rv;
	}
	fn n_state(&self) -> usize { 8 }
	fn next(&mut self) {
		if self.idx < 8 {
			self.idx = self.idx + 1
		}
	}
	fn done(&self) -> bool { return self.idx >= 7; }
	fn reset(&mut self) { self.idx = 0; }
	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "cstr{{{} of {}}}", self.idx, 8)
	}
	fn clone(&self) -> Box<Generator> {
		Box::new(GenCString{idx: self.idx, printable: self.printable.clone(),
		                    control: self.control.clone()})
	}
}

// GenIgnore creates a generator that wraps around another generator and
// ignores one of its states.
pub struct GenIgnore {
	subgen: Box<Generator>,
	ign: usize, // the index to ignore
	idx: usize, // the index we are currently at.  should never be == to ign
	name: String, // name of the generator, for name().  client gives this to us.
}

impl GenIgnore {
	// Creates a new generator named 'nm' that ignores 'gen's 'index' element.
	pub fn new(gen: Box<Generator>, index: usize, nm: &str) -> GenIgnore {
		let curidx = if index == 0 { 1 } else { 0 };
		return GenIgnore{ subgen: gen.clone(), ign: index, idx: curidx,
		                  name: nm.to_string() };
	}
}
impl Generator for GenIgnore {
	fn name(&self) -> String { self.name.clone() }
	// This is wrong.  If we're supposed to ignore index 0, but the subgen uses
	// self.value() in ITS implementation of decl(), then we'll initialize it
	// with the supposed-to-be-ignored 0 value().
	fn decl(&self, varname: &str) -> String {
		assert!(self.ign != 0);
		self.subgen.decl(varname)
	}
	fn value(&self) -> String { self.subgen.value() }

	fn next(&mut self) {
		self.subgen.next();
		// also keep track locally:
		if self.idx < self.subgen.n_state()-1 {
			self.idx = self.idx + 1
		}
		// ... and if the local value is the ignore value, .next() again:
		if self.idx == self.ign {
			self.next()
		}
	}
	fn done(&self) -> bool {
		return self.idx >= self.subgen.n_state()-1;
	}
	fn n_state(&self) -> usize { self.subgen.n_state()-1 }
	fn reset(&mut self) {
		self.idx = if self.ign == 0 { 1 } else { 0 };
		self.subgen.reset();
		if self.ign == 0 {
			self.subgen.next();
		}
	}

	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "ign{{{} of {}}}", self.idx, self.n_state()-1)
	}
	fn clone(&self) -> Box<Generator> {
		Box::new(GenIgnore::new(self.subgen.clone(), self.ign, &self.name))
	}
}

#[derive(Debug)]
pub enum Variant {
	Func(String, Vec<Box<Generator>>),
	Field(String, Box<Generator>),
}
// Manually implement clone because of the Box'd trait.
impl Clone for Variant {
	fn clone(&self) -> Variant {
		match *self {
			Variant::Func(ref v, ref gens) => {
				let gencopy = gens.iter().map(|gen| (*gen).clone()).collect();
				Variant::Func(v.clone(), gencopy)
			},
			Variant::Field(ref fld, ref gen) => {
				Variant::Field(fld.clone(), gen.deref().clone())
			},
		}
	}
}

// a generator for a hypothetical graph API.
pub struct FauxGraph {
	var: String,
	variants: Vec<Variant>,
	idx: usize,
	initializer: Expression,
}
impl FauxGraph {
	pub fn new(varname: String, init: &Expression, vars: &Vec<Variant>) -> Self {
		FauxGraph{
			var: varname,
			variants: vars.clone(),
			idx: 0,
			initializer: init.clone(),
		}
	}
}

// Let's say we have three variants with the prototypes:
//   foo(nitz*, enum A),
//   bar(nitz*, enum B),
//   baz(nitz*, enum C)
// Each call could be present or not, and there could be any number of
// specific things passed for 'A', 'B', or 'C' that I'll refer to as e.g.
// 'len(A)'.
// The number of combinations is simple: 2^n (think of foo, bar, and baz
// as bits that can be enabled or not).  But there are multiple variants
// within each of those "bits": there are len(B) options if the bar()
// function is "on".
// Recognize that the choice of a possible value for enum B is independent
// to any other choice.  That is, choosing 'B1' is independent of whether
// we've chosen 'A0' or 'A1', and even independent of whether foo() will be
// called or not.
// A specific instance of such a generated program is a set of sets of
// bitstrings.  We concatenate all the bitstrings for ease of reasoning, so we
// just have a set of bits.
impl Generator for FauxGraph {
	fn name(&self) -> String { "gen:faux-graph".to_string() }
	fn decl(&self, varname: &str) -> String {
		assert!(varname == self.var);
		let mut rv = String::new();
		write!(&mut rv, "{} {}", self.initializer.extype().name(),
		       varname).unwrap();

		// HACK: We're going to need to take the program as an argument eventually.
		// For now just create a fake one.
		use api;
		let foo: api::Program = api::Program::new(&vec![], &vec![]);
		use stmt::Code;
		let mut strm : Vec<u8> = Vec::new();
		self.initializer.codegen(&mut strm, &foo).unwrap();
		write!(&mut rv, " = {}", String::from_utf8(strm).unwrap()).unwrap();
		return rv;
	}

	fn value(&self) -> String {
		// This is a multi-step process.  Earlier we said that each variant has its
		// own bit string that gets concatenated together to form the value.  In
		// practice, this value() only deals with the detail of whether each
		// variant is on or off.  The detail of what specific value that variant
		// will have is handled by calling value() on the sub-generator.

		// self.idx is a bitmask that tells us which variants should be called.
		// We run over every possible bit in a usize: if that bit is set, then we
		// generate the code for that variant.
		let numbits = ::std::mem::size_of::<usize>() * 8;
		// Small optimization: if we have 13u64 == 1101b variants, then 10000b ==
		// 16u64 aka the next power of two is an upper bound on possible unique
		// selections of variants.  We can use it as an early out, then.
		let higher = match self.variants.len().checked_next_power_of_two() {
			None => usize::max_value(),
			Some(h) => h,
		};
		let mut rv = String::new();
		for i in 0..numbits {
			let bit = 1usize << i;
			if bit >= higher {
				break;
			}
			if (self.idx & bit) > 0 {
				match self.variants[bit] {
					Variant::Func(ref func, ref args) => {
						write!(&mut rv, "\t{}({}", func, self.var).unwrap();
						for arg in args.iter() {
							write!(&mut rv, ", {}", arg.deref().value()).unwrap();
						}
						write!(&mut rv, ");\n").unwrap();
					},
					Variant::Field(ref fld, ref rhs) => {
						write!(&mut rv, "\t{}.{} = {};\n", self.var, fld,
						       rhs.deref().value()).unwrap();
					},
				};
			}
		}
		rv
	}
	fn next(&mut self) {
		self.idx = self.n_state().min(self.idx+1);
	}
	fn done(&self) -> bool {
		return self.idx >= self.n_state()
	}

	// The number of states in the FauxGraph test generator.
	fn n_state(&self) -> usize {
		// We first compute the number of bits in the concatenated bit strings.
		// This is simply the sum of bits in all variants.
		let n_per_subgen: Vec<usize> = self.variants.iter().map(|v|
			match *v {
				Variant::Func(_, ref args) =>
					args.iter().fold(0, |accum, arg| accum + arg.deref().n_state()),
				Variant::Field(_, ref gen) => gen.n_state(),
			}
		).collect();
		let nbits: usize = n_per_subgen.iter().fold(0, |accum, ns| accum+ns);
		// Add in 1 bit per variant, to account for the case where the function is
		// not called / the field is not set.
		let nbits: usize = nbits + self.variants.len();

		// 2^nsubgen is the number of states we have.
		let two: usize = 2;
		return two.pow(nbits as u32);
	}

	fn reset(&mut self) { self.idx = 0; }

	fn dbg(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "FauxGraph{{{}, {} of {}}}", self.var, self.idx,
		       self.n_state())
	}
	// Workaround because we can't clone() a trait, or a Box<> of one.
	fn clone(&self) -> Box<Generator> {
		Box::new(FauxGraph{var: self.var.clone(), variants: self.variants.clone(),
		                   idx: self.idx, initializer: self.initializer.clone()})
	}
}

#[cfg(test)]
mod test {
	use expr::Expression;
	use function::Function;
	use variable::{generator, Generator};
	use typ::{Native, Type};

	macro_rules! genmatch {
		($gtype:expr, $gname:expr) => (
			let gen: Box<Generator> = generator(&$gtype);
			assert_eq!(gen.name(), $gname);
		)
	}

	#[test]
	fn gen_native() {
		genmatch!(Type::Builtin(Native::I32), "std:I32orig");
	}

	#[test]
	fn gen_ignore_null_cstring() {
		use super::*;
		let cstype = Type::Pointer(Box::new(Type::Builtin(Native::Character)));
		let mut cs = GenCString::create(&cstype);
		let mut nncs = GenIgnore::new(cs.clone(), 0, "std:cstring:nonnull");
		assert_eq!(nncs.n_state(), cs.n_state()-1);
		for _ in 0..cs.n_state()-2 {
			nncs.next(); cs.next();
		}
		assert!(nncs.done());
		assert!(!cs.done());
		nncs.reset();
		// Ideally we would not verify the order that the generator creates these,
		// but that would complicate the test code significantly.
		let zerolen = nncs.value(); nncs.next();
		let normal1 = nncs.value(); nncs.next();
		let special1 = nncs.value(); nncs.next();
		let normal_n = nncs.value(); nncs.next();
		let special_n = nncs.value(); nncs.next();
		let mixed_n = nncs.value(); nncs.next();
		let longstr = nncs.value(); nncs.next();
		println!("zerolen: '{}'", zerolen);
		assert_eq!(zerolen, "\"\"".to_string());
		assert_eq!(normal1.len(), 3);
		assert_eq!(special1.len(), 3);
		assert!(normal_n.len() > 3);
		assert!(special_n.len() > 3);
		assert!(mixed_n.len() > 3);
		assert!(longstr.len() > 128);
	}

	#[test]
	fn faux_graph_states() {
		use variable::{natgenerator, FauxGraph, Variant};
		let gt = Type::Struct("graph_t".to_string(), vec![]);
		let rvtype = Type::Pointer(Box::new(gt));
		let initfunc = Function::new("graph_create", &rvtype, &vec![]);
		let initexpr = Expression::FqnCall(initfunc, vec![]);
		let methods = vec![
			Variant::Func("foo".to_string(), vec![]),
			Variant::Func("bar".to_string(), vec![]),
			Variant::Func("baz".to_string(), vec![]),
			Variant::Field("foo2".to_string(), natgenerator(&Native::I32)),
		];
		let fg = FauxGraph::new("grph".to_string(), &initexpr, &methods);
		assert_eq!(fg.decl("grph"), "struct graph_t* grph = graph_create()");
		// 3 functions with 0 args => 3 bits
		// one field with 7 subgen states and one "enabled" bit => 8 bits
		// == 11 bits => 2^11 == 2048
		assert_eq!(fg.n_state(), 2048);
	}

	#[test]
	fn faux_graph_iter_terminates() {
		use variable::{natgenerator, FauxGraph, Variant};
		let gt = Type::Struct("graph_t".to_string(), vec![]);
		let rvtype = Type::Pointer(Box::new(gt));
		let initfunc = Function::new("graph_create", &rvtype, &vec![]);
		let initexpr = Expression::FqnCall(initfunc, vec![]);
		let methods = vec![
			Variant::Func("foo".to_string(), vec![natgenerator(&Native::I32)]),
		];
		let mut fg = FauxGraph::new("grph".to_string(), &initexpr, &methods);
		while !fg.done() {
			fg.next();
		}
		assert!(fg.done());
	}
}
