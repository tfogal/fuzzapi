extern crate rand;
use std::ptr;
use rand::distributions::{IndependentSample, Range};

// A class of types.
pub trait TypeClass<T> {
	fn n(&self) -> usize;
	fn value(&self, class: usize) -> T;
}

// Specialization is not yet stable in rust.  Thus the types are not type
// parameters but baked into the type name.  Sigh.
pub struct TC_U8 {}
pub struct TC_U16 {}
/*...*/
pub struct TC_I32 {}

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
