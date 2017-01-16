pub struct BitVector {
	bits: Box<[u8]>
}

impl BitVector {
	pub fn new(n: usize) -> BitVector {
		let mut v = Vec::with_capacity((n/8)+1);
		unsafe { v.set_len((n/8)+1); }
		return BitVector{bits: v.into_boxed_slice() };
	}

	pub fn set(&mut self, n: u64) {
		let byte = n / 8;
		let bit = 1u8 << ((n % 8) as u8);
		self.bits[byte as usize] |= bit;
	}

	pub fn clear(&mut self, n: u64) {
		let byte = n / 8;
		let bit = 1u8 << ((n % 8) as u8);
		let inverse = !bit;
		self.bits[byte as usize] &= inverse;
	}

	pub fn query(&self, n: u64) -> bool {
		let byte = n / 8;
		let bit = 1u8 << ((n % 8) as u8);
		return self.bits[byte as usize] & bit > 0
	}
}

#[cfg(test)]
mod tests {
	use super::BitVector;

#[test]
fn bvsimple() {
	let mut bv = BitVector::new(42);
	bv.set(19);
	for i in 0 .. 41 {
		if i == 19 {
			assert!(bv.query(i))
		} else {
			assert_eq!(bv.query(i), false);
		}
	}
}

#[test]
fn bvall() {
	let mut bv = BitVector::new(9);
	for i in 0..9 {
		bv.set(i);
	}
	for i in 0..9 {
		assert!(bv.query(i));
	}
}

#[test]
fn bvsetclear() {
	let mut bv = BitVector::new(300);
	bv.set(56);
	bv.set(55);
	bv.clear(56);
	assert!(bv.query(55));
	assert_eq!(bv.query(56), false);
}

#[test]
fn bvsetboundary() {
	let mut bv = BitVector::new(42);
	bv.set(7);
	bv.set(8);
	bv.set(9);
	assert!(bv.query(7));
	assert!(bv.query(8));
	assert!(bv.query(9));

	bv.clear(7);
	assert_eq!(bv.query(7), false);
	assert_eq!(bv.query(8), true);
	assert_eq!(bv.query(9), true);
	bv.set(7);

	bv.clear(8);
	assert_eq!(bv.query(7), true);
	assert_eq!(bv.query(8), false);
	assert_eq!(bv.query(9), true);
	bv.set(8);

	bv.clear(9);
	assert_eq!(bv.query(7), true);
	assert_eq!(bv.query(8), true);
	assert_eq!(bv.query(9), false);
	bv.set(9);
	assert_eq!(bv.query(9), true);
}

}
