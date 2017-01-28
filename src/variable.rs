use function::*;

pub enum Source<'a> {
	Free,
	Parameter(&'a Function, usize), // function + parameter index it comes from
	ReturnValue(&'a Function),
}

pub enum Use<'a> {
	Nil, // isn't used.
	Argument(&'a Function, usize), // function + parameter index it comes from
}

pub trait Free {
	fn name(&self) -> String;
	// The number of states this has.
	fn n_state(&self) -> usize;
	// Apply the next state.
	fn next(&mut self);
}
