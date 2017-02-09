// 'tc'---TypeClass---holds the type information for every type we support.
// Most fuzzing techniques consider every "free" input to be able to take on
// *any* input and use randomness to try to explore that space.  The search
// space is then so large that it is practically infinite.  Some fuzzers will
// use e.g. genetic algorithms to bias that search space towards "fruitful"
// areas.
//
// We try something a bit different, using these type classes so that we can
// have a defined "end", a notion that future testing is unlikely to produce
// significant results.  The idea is that while (say) a 'uint32_t' can
// technically have 2^32 values, testing *all* of those values is unlikely to
// be valuable.  Rather, there are a number of conceivable errors a program
// could make with such values: assuming it will never be 0, assuming it does
// not cause overflow, etc.
//
// The idea is that while fuzzing may still be expensive---especially for large
// APIs, the state space is the intersection of all "free" input
// variables---there is nonetheless a point that you might label it as
// "verified".
//
// Despite the above, we still utilize randomness.  Many TypeClasses have
// values that we consider more of a range than an exact value.  For example,
// an int8_t might have 255 as one type class, and "near but less than 255"
// as another type class.  When choosing a value for the latter state, we
// utilize randomness.  Thus subsequent runs with the exact same inputs may
// find different sets of bugs.
extern crate rand;
use std::collections::btree_map::BTreeMap;
use rand::distributions::{IndependentSample, Range};
use typ::*;

// A class of types.
pub trait TypeClass<T> {
	fn n(&self) -> usize;
	fn value(&self, class: usize) -> T;
}

// Specialization is not yet stable in rust.  Thus the types are not type
// parameters but baked into the type name.  Sigh.

#[allow(non_camel_case_types, dead_code)]
pub struct TC_U8 {}
#[allow(non_camel_case_types, dead_code)]
pub struct TC_U16 {}
/*...*/
#[allow(non_camel_case_types)]
pub struct TC_I32 {}
#[allow(non_camel_case_types)]
pub struct TC_Pointer {}
#[allow(non_camel_case_types)]
pub struct TC_Enum {
	values: Vec<u32>
}

// A u8 has four classes: 0, near 0, and near 255.  The idea is that 0s bring
// out all sorts of nonsense; near 0 is a "normal" case.  Near 255 and 255 will
// highlight overflow as well as cases that might inappropriately cast to
// signed or similar.
impl TypeClass<u8> for TC_U8 {
	fn n(&self) -> usize { return 4; }
	fn value(&self, class: usize) -> u8 {
		// UGH.  Getting a static Range<x> is a nightmare.  For now we'll just
		// reallocate every damn call.
		let mut rng: rand::ThreadRng = rand::thread_rng();
		let du8_1_128 = Range::new(1, 128);
		let du8_129_254 = Range::new(129, 254);
		match class {
			0 => 0,
			1 => du8_1_128.ind_sample(&mut rng),
			2 => du8_129_254.ind_sample(&mut rng),
			3 => 255,
			_ => panic!("invalid type class {} given for u8!", class),
		}
	}
}

impl TypeClass<u16> for TC_U16 {
	fn n(&self) -> usize { return 4; }
	fn value(&self, class: usize) -> u16 {
		let mut rng: rand::ThreadRng = rand::thread_rng();
		let du16_1_32767 = Range::new(1, 128);
		let du16_32768_65534 = Range::new(129, 254);
		match class {
			0 => 0,
			1 => du16_1_32767.ind_sample(&mut rng),
			2 => du16_32768_65534.ind_sample(&mut rng),
			3 => 65535,
			_ => panic!("invalid type class {} given for u16!", class),
		}
	}
}

impl TC_I32 {
	pub fn new() -> Self {
		TC_I32 {}
	}
}
impl TypeClass<i32> for TC_I32 {
	fn n(&self) -> usize { return 7; }
	fn value(&self, class: usize) -> i32 {
		let mut rng: rand::ThreadRng = rand::thread_rng();
		let du_neg_large = Range::new(i32::min_value()+1, i32::min_value()/2);
		let du_neg_small = Range::new(i32::min_value()/2+1, -1);
		let du_pos_small = Range::new(1, i32::max_value()/2);
		let du_pos_large = Range::new(i32::max_value()/2+1, i32::max_value()-1);
		match class {
			0 => i32::min_value(),
			1 => du_neg_large.ind_sample(&mut rng),
			2 => du_neg_small.ind_sample(&mut rng),
			3 => 0,
			4 => du_pos_small.ind_sample(&mut rng),
			5 => du_pos_large.ind_sample(&mut rng),
			6 => i32::max_value(),
			_ => panic!("invalid type class {} given for i32!", class),
		}
	}
}

impl TC_Enum {
	// An enum maps strings to their actual values.  But we don't actually care
	// about the strings, so just pull out all the values and keep those.
	pub fn new(ty: &Type) -> Self {
		let enm: BTreeMap<String, u32> = match ty {
			&Type::Enum(_, ref map) => { map.clone() },
			x => panic!("Tried to give a non-enum {:?} to TC_Enum!", x),
		};
		TC_Enum{
			values: enm.values().cloned().collect(),
		}
	}
}

impl TypeClass<i32> for TC_Enum {
	fn n(&self) -> usize { self.values.len() }
	// Because we already pulled out the values, we can just use the class as an
	// index into that list.
	fn value(&self, class: usize) -> i32 {
		assert!(class < self.values.len());
		return self.values[class] as i32;
	}
}

impl TC_Pointer {
	pub fn new() -> Self {
		TC_Pointer{}
	}
}

// Pointers are pretty simple: null-initialized or not.
impl TypeClass<usize> for TC_Pointer {
	fn n(&self) -> usize { 2 }
	fn value(&self, class: usize) -> usize {
		let mut rng: rand::ThreadRng = rand::thread_rng();
		let arb = Range::new(1, usize::max_value()-1);
		match class {
			0 => 0,
			1 => arb.ind_sample(&mut rng),
			_ => panic!("invalid class {} given for TC_Pointer", class),
		}
	}
}
