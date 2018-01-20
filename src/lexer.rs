use std::str::CharIndices;

pub type Spanned<Loc, Tok> = (Loc, Tok);
/*
pub type Spanned<Loc, Tok, Error> = Result<(Loc, Tok), Error>;
pub enum LexError {
}
*/

#[derive(Debug, PartialEq)]
pub enum Tok {
	Str(String),
	Integer(i64),
	Floating(f64),
	Error,
}


#[derive(Debug, PartialEq)]
enum State {
	/// Initial state.  Also where we jump to if we're open to any option.
	Start,
	/// Lexing an integer.  Might actually be an FP number where we just haven't
	/// hit the '.' or similar yet.
	Integer,
	/// Lexing a floating point number.
	Floating,
	/// Lexing a string, i.e. something that started with '"'.
	String,
	/// A single word token, i.e. delimited by whitespace.  The input:
	///     extern "C" foo
	/// would be parsed into the sequence [Word(extern), String(C), Word(foo)]
	Word,
	/// Catch-all for "odd" characters, mostly punctuation ('<', '/', etc.).
	/// Note that these are still returned as a Tok::Str.
	Other,
	Error,
}

pub struct Lexer<'input> {
	chars: CharIndices<'input>,
	state: State,
	tok: Vec<char>,
}

impl<'input> Lexer<'input> {
	pub fn new(input: &'input str) -> Self {
		Lexer { chars: input.char_indices(), state: State::Start, tok: vec![]}
	}

	fn other(&self, ch: char) -> bool {
		match ch {
			'!'...'/' | ':'...'@' | '['...'`' | '{'...'~' => true,
			_ => false,
		}
	}
	fn word(&self, ch: char) -> bool {
		match ch {
			'A'...'Z' | 'a'...'z' | '_' | '0'...'9' => true,
			_ => false,
		}
	}

	/// Shifts the given character.  This may mean that a token is emitted; if
	/// so, it is reduced and returned.
	fn shift(&mut self, _pos_: usize, ch: char) -> Option<Tok> {
		match ch {
			// Once in error, always in error.
			_ if self.state == State::Error => Some(Tok::Error),
			// If Integer or convertible to it ...
			'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
				if self.state == State::Start || self.state == State::Integer => {
					self.state = State::Integer;
					self.tok.push(ch);
					None
				},
			// If Integer but we see a char indicating FP ...
			'.' | 'e' | 'E' | 'f' if self.state == State::Integer => {
				self.state = State::Floating;
				self.tok.push(ch);
				None
			},
			// If FP and we see another FP char ...
			'0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'f'
				if self.state == State::Floating => {
				self.tok.push(ch);
				None
			},
			// Whitespace will end any kind of number, a word, and our 'other' state.
			' ' | '\t' | '\n' | '\r'
				if self.state == State::Integer || self.state == State::Floating ||
				   self.state == State::Word || self.state == State::Other => {
					assert!(self.tok.len() > 0); // else how did we get to these states?
					let v = self.reduce();
					self.clear();
					Some(v)
				},
			// '"' starts a string if we're in a state to be able to accept that.
			'"' if self.state == State::Start => {
				assert!(self.tok.len() == 0); // start state but non-empty len?
				self.state = State::String;
				None
			},
			// '"' ends a string if we're already in one.
			'"' if self.state == State::String => {
				let v = self.reduce();
				self.clear();
				Some(v)
			},
			// anything else in a string is just a continuation of the string.
			'\t' ... '\r' | ' ' | '!' | '#' ... '~'
				if self.state == State::String => {
				self.tok.push(ch);
				None
			},
			// "regular" characters will start a word.  we don't use self.word() here
			// so that we can avoid having 0--9 as a word start.
			'a' ... 'z' | 'A' ... 'Z' if self.state == State::Start => {
				assert_eq!(self.tok.len(), 0);
				self.state = State::Word;
				self.tok.push(ch);
				None
			},
			// "regular" characters will continue a word, too.
			wd if self.word(wd) && self.state == State::Word => {
				self.tok.push(ch);
				None
			},
			// other characters will stop a word and start an other.
			oth if self.other(oth) && self.state == State::Word => {
				let v = self.reduce();
				self.clear();
				self.tok.push(ch);
				self.state = State::Other;
				Some(v)
			},
			// @todo fixme: 'other' terminals should chain in certain cases.  for
			// example, ">=" should be returned as a single token.  Note it's not
			// enough to just return the tokens separately, because then the caller
			// cannot differentiate between ">=" and "> =", the former of which may
			// be valid whilst the latter is always garbage.

			// Just about any character while in an 'other' state will end the
			// current terminal and start another 'other'.
			oth if self.other(oth) && self.state == State::Other => {
				let v = self.reduce();
				self.tok.clear();
				self.tok.push(ch);
				Some(v)
			},
			// a word char in 'other' means to return the other and start a word.
			wd if self.word(wd) && self.state == State::Other => {
				let v = self.reduce();
				self.tok.clear();
				self.tok.push(ch);
				self.state = State::Word;
				Some(v)
			},
			// a word char when in word state just continues the word.
			wd if self.word(wd) && self.state == State::Word => {
				let v = self.reduce();
				self.tok.clear();
				self.tok.push(ch);
				self.state = State::Word;
				Some(v)
			},
			// other character in the start state reduces immediately and returns to
			// the start state.
			oth if self.other(oth) && self.state == State::Start => {
				assert!(self.tok.len() == 0);
				self.tok.push(ch);
				self.state = State::Other;
				let v = self.reduce();
				self.tok.clear();
				self.state = State::Start;
				Some(v)
			},
			// If it's just whitespace, just eat/ignore it.
			'\t' ... '\r' | ' ' if self.state == State::Start  => None,
			// ... anything else, bail.
			_ => {
				println!("'{}' in state {:?}; error.", ch, self.state);
				self.state = State::Error; Some(Tok::Error)
			}
		}
	}

	/// Takes whatever's currently in self.tok and reduces it to a token based on
	/// the current state.
	/// To keep this immutable, unlike a true reduce we do not clean out the
	/// current state.
	fn reduce(&self) -> Tok {
		match self.state {
			State::Start | State::Error => Tok::Error,
			State::Integer => {
				let s: String = self.tok.iter().cloned().collect();
				match s.parse::<i64>() {
					Err(e) => panic!("cannot parse {} as i64: {}", s, e),
					Ok(x) => Tok::Integer(x),
				}
			},
			State::Floating => {
				let s: String = self.tok.iter().cloned().collect();
				match s.parse::<f64>() {
					Err(e) => panic!("cannot parse {} as f64: {}", s, e),
					Ok(f) => Tok::Floating(f),
				}
			},
			State::String => Tok::Str(self.tok.iter().cloned().collect()),
			State::Other => Tok::Str(self.tok.iter().cloned().collect()),
			State::Word => Tok::Str(self.tok.iter().cloned().collect()),
		}
	}

	/// Resets the current state.
	fn clear(&mut self) {
		self.tok.clear();
		self.state = State::Start;
	}
}

impl<'input> Iterator for Lexer<'input> {
	type Item = Spanned<usize, Tok>;

	/// Grabs the next token.
	fn next(&mut self) -> Option<Self::Item> {
		// This is recursive.  If a token is ready, we return it.  Otherwise, we
		// shift a character and then call ourselves.
		match self.chars.next() {
			None => { // EOF special case.
				// if we do not have a token in progress, just indicate EOF with None.
				if self.tok.len() == 0 {
					return None
				}
				// if we DO have a token in progress, make a real token out of it and
				// then clear it so that subequent calls will return None.
				let rv: Tok = self.reduce();
				self.tok.clear();
				let fixme_should_be_cur_pos = 0;
				Some((fixme_should_be_cur_pos, rv))
				//Some(Ok((fixme_should_be_cur_pos, rv)))
			},
			Some((i, ch)) => {
				match self.shift(i, ch) {
					//Some(tk) => Some(Ok((i, tk))),
					Some(tk) => Some((i, tk)),
					None => self.next(),
				}
			},
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn quick_brown_fox() {
		let s = "The quick brown fox jumps over the lazy brown dog.";
		let mut lex = Lexer::new(s);
		assert_eq!(lex.next().unwrap().1, Tok::Str("The".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("quick".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("brown".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("fox".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("jumps".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("over".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("the".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("lazy".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("brown".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("dog".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(".".to_string()));
		// subsequent calls should be None repeatedly, i.e. eof.
		assert_eq!(lex.next(), None);
		assert_eq!(lex.next(), None);
		assert_eq!(lex.next(), None);
	}

	#[test]
	fn extern_c_func() {
		let s = "extern \"C\" foo(int x, char y);";
		let mut lex = Lexer::new(s);
		assert_eq!(lex.next().unwrap().1, Tok::Str("extern".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("C".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("foo".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("(".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("int".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("x".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(",".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("char".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("y".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(")".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(";".to_string()));
		assert_eq!(lex.next(), None);
		assert_eq!(lex.next(), None);
		assert_eq!(lex.next(), None);
	}

	#[test]
	fn builtin_decls() {
		let s = "int x;\nuint32_t y;\nconst char* name = \"foo\";\n";
		let mut lex = Lexer::new(s);
		assert_eq!(lex.next().unwrap().1, Tok::Str("int".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("x".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(";".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("uint32_t".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("y".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(";".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("const".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("char".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("*".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("name".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("=".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str("foo".to_string()));
		assert_eq!(lex.next().unwrap().1, Tok::Str(";".to_string()));
		assert_eq!(lex.next(), None);
	}
}
