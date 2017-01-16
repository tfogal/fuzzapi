//mod bitvector;
use bitvector::*;

pub struct Bloom {
	bv: BitVector,
	/* set of function pointers for all the hash functions */
	hashes: [fn(u64) -> u16; 4],
}

fn hash1(x: u64) -> u16 {
	return (x % 65535) as u16;
}
fn hash2(x: u64) -> u16 {
	return (((x >> 24) * (x >> 24) + (x >> 12) + (x >> 6)) % 65535) as u16;
}
fn hash3(x: u64) -> u16 {
	return ((x ^ 0xacccf956a9410cab) % 65535) as u16;
}
fn hash4(x: u64) -> u16 {
	return ((x ^ 0x137ab591abdfca56) % 65535) as u16;
}

impl Bloom {
	pub fn new() -> Self {
		Bloom { bv: BitVector::new(65535), hashes: [hash1, hash2, hash3, hash4] }
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
		for i in 0..65535 {
			assert_eq!(bl.query(i), false);
		}
	}

	/* should probably add a test for which the add()ed value collides with one
	 * or more of the hash functions. */
}
