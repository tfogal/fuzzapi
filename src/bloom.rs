use bitvector::*;

pub struct Bloom {
	bv: BitVector,
	/* set of function pointers for all the hash functions */
	hashes: [fn(u64) -> u16; 4],
}

static MAX_BITS: u64 = 65535;

fn hash1(x: u64) -> u16 {
	return (x % MAX_BITS) as u16;
}
fn hash2(x: u64) -> u16 {
	let t = ((x >> 24).wrapping_mul(x >> 24) + (x >> 12) + (x >> 6)) % MAX_BITS;
	return t as u16;
}
fn hash3(x: u64) -> u16 {
	return ((x ^ 0xacccf956a9410cab) % MAX_BITS) as u16;
}
fn hash4(x: u64) -> u16 {
	return ((x ^ 0x137ab591abdfca56) % MAX_BITS) as u16;
}

impl Bloom {
	pub fn new() -> Self {
		Bloom { bv: BitVector::new(MAX_BITS as usize),
		        hashes: [hash1, hash2, hash3, hash4] }
	}

	pub fn add(&mut self, v: u64) {
		for h in self.hashes.iter() {
			self.bv.set(h(v) as u64);
		}
	}

	pub fn query(&self, v: u64) -> bool {
		for h in self.hashes.iter() {
			if !self.bv.query(h(v) as u64) {
				return false;
			}
		}
		return true;
	}
}

#[cfg(test)]
mod test {
	use super::Bloom;

	#[test]
	fn bloomsimple() {
		let mut bl = Bloom::new();
		bl.add(42);
		assert_eq!(bl.query(42), true);
	}

	#[test]
	fn blnone() {
		let bl = Bloom::new();
		for i in 0..MAX_BITS {
			assert_eq!(bl.query(i), false);
		}
	}

	/* should probably add a test for which the add()ed value collides with one
	 * or more of the hash functions. */
}
