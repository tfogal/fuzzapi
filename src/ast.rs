pub enum Include {
  Local(String),
  System(String)
}

pub struct Typedef {
	pub from: String,
	pub to: String,
}
