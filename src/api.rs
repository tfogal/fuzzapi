// This provides a type system and API for parsing code.
//
// The type system here mirrors typ::Type, but we use more strings here
// to reference types instead of fully instantiating them.  This makes
// sense for parsing, because it lets us parse without worrying too
// much about semantics, and thereby importantly means we do less error
// handling during parsing and more during subsequent semantic analysis.
use std;
use function;
use stmt;
use typ::{EnumValue, Native, Type};
use opcode::{BinOp, Op, UOp};
use variable;
use variable::Generator;

#[derive(Clone, Debug)]
pub enum DeclType {
	Basic(Type),
	Struct(String, Vec<UDTDecl>),
	Enum(String, Vec<EnumValue>),
	StructRef(String),
	EnumRef(String),
}

#[derive(Clone, Debug)]
pub struct UDTDecl {
	pub name: String,
	pub ty: DeclType,
}

#[derive(Clone, Debug)]
pub struct FreeVarDecl {
	pub name: String,
	pub genname: String,
	pub ty: DeclType, // Struct(...) and Enum(...) are not valid, but *Refs are.
}

#[derive(Clone, Debug)]
pub struct FuncDecl {
	pub name: String,
	pub retval: DeclType,
	pub arguments: Vec<DeclType>,
}

#[derive(Clone, Debug)]
pub enum Declaration {
	// "Constrained" variables are the opposite of "free" variables; they might
	// vary at runtime of the generated program, but the initial value is not a
	// choice of the fuzzer.
	Constrained(String /* name */, DeclType),
	Free(FreeVarDecl),
	Function(FuncDecl),
	UDT(DeclType), // Error if the DeclType is not a Struct || Enum!
}

#[derive(Clone, Debug)]
pub enum Expr {
	VarRef(UOp, String /* varname */),
	IConst(String),
	FConst(String),
	Call(String /* funcname */, Box<Vec<Expr>> /* args */),
	Compound(Box<Expr>, BinOp, Box<Expr>),
	Field(String, String),
}
#[derive(Clone, Debug)]
pub enum Stmt {
	Basic(Expr),
	Declaration(Declaration),
	Assignment(Expr /* LHS */, Expr /* RHS */),
	Verify(Expr),
	Constraint(Expr),
	If(Expr, Box<Vec<Stmt>>),
	While(Expr, Box<Vec<Stmt>>),
}

#[derive(Debug)]
pub struct Symbol {
	pub name: String,
	pub generator: Box<variable::Generator>, // actual, used generator.
	pub typ: Type,
}
impl PartialEq for Symbol {
	fn eq(&self, other: &Symbol) -> bool {
		return self.name == other.name && self.typ == other.typ;
	}
}
impl Eq for Symbol {}
impl Clone for Symbol {
	fn clone(&self) -> Self {
		Symbol{name: self.name.clone(), generator: self.generator.clone(),
		       typ: self.typ.clone()}
	}

	#[allow(unused_variables)]
	fn clone_from(&mut self, src: &Self) {
		unimplemented!();
	}
}

// Program object, represents the state of the abstract program given to us by
// the user.
#[derive(Debug)]
pub struct Program {
	pub declarations: Vec<Declaration>,
	// The AST is what we parsed out from the user.  Essentially everything is
	// referenced via a string.  Yes, technically it isn't a tree, but that's
	// because enums/matches in Rust get us all the branching we need.
	ast: Vec<Stmt>,
	// The statements are something we can actually codegen from.  The client
	// should not write into this: rather, the client creates the AST, and as we
	// resolve entries from the AST to actual objects (i.e. references to a
	// Symbol in self.symtab instead of a string variable name), we will insert
	// those objects into 'statements'.
	pub statements: Vec<stmt::Statement>,
	symtab: Vec<Symbol>,
	typetab: Vec<Type>,
	// copy of generator list.  Expected users will clone() out of it to create
	// the real/used Generators (that live in the symbol table).
	genlist: Vec<Box<variable::Generator>>,
}

impl Program {
	pub fn new(decls: &Vec<Declaration>, stmts: &Vec<Stmt>)
		-> Program {
		Program{declarations: (*decls).clone(), statements: Vec::new(),
		        ast: (*stmts).clone(),
		        symtab: Vec::new(), typetab: Vec::new(), genlist: Vec::new()}
	}

	pub fn set_generators(&mut self, gens: &Vec<Box<Generator>>) {
		self.genlist.clear();
		for g in gens {
			self.genlist.push((*g).clone());
		}
	}

	pub fn symlookup<'a>(&'a self, symname: &str) -> Option<&'a Symbol> {
		for s in self.symtab.iter() {
			if s.name == symname {
				return Some(s);
			}
		}
		None
	}

	fn genlookup(&self, ty: &Type, genname: &str) -> Option<Box<Generator>> {
		#[allow(non_snake_case)]
		let GENNAME = genname.to_string().to_uppercase();
		for gen in self.genlist.iter() {
			if gen.name().to_uppercase() == GENNAME {
				return Some((*gen).clone());
			}
		}
		// if we didn't find any in the list, try to create one from the type.
		Some(variable::generator(ty))
	}

	// Creates an entry in the symtable for every variable in the program.
	fn populate_symtable(&mut self) {
		for ref decl in self.declarations.iter() {
			match **decl {
				Declaration::Free(ref fvd) => {
					let ty = type_from_decl(&fvd.ty, &self.typetab);
					let gen = self.genlookup(&ty, &fvd.genname).unwrap();
					let sym = Symbol{name: fvd.name.clone(), generator: gen, typ: ty};
					self.symtab.push(sym);
				},
				Declaration::Constrained(ref nm, ref decl) => {
					let ty = type_from_decl(decl, &self.typetab);
					let gen = variable::generator_single(&ty);
					let sym = Symbol{name: nm.clone(), generator: gen, typ: ty};
					self.symtab.push(sym);
				},
				Declaration::Function(ref fqn) => {
					let ty = type_from_decl(&fqn.retval, &self.typetab);
					use variable;
					let gen = Box::new(variable::GenNothing{});
					let sym = Symbol{name: fqn.name.clone(), generator: gen, typ: ty};
					self.symtab.push(sym);
				},
				Declaration::UDT(_) => (),
			}
		}
		for ref stmt in self.ast.iter() {
			match **stmt {
				Stmt::Declaration(ref decl) => {
					match *decl {
						Declaration::Free(ref fvd) => {
							let ty = type_from_decl(&fvd.ty, &self.typetab);
							let gen = self.genlookup(&ty, &fvd.genname).unwrap();
							let sym = Symbol{name: fvd.name.clone(), generator: gen,
							                 typ: ty.clone()};
							self.symtab.push(sym);
						},
						Declaration::Constrained(ref nm, ref decltype) => {
							// The only difference between a constrained variable declaration
							// and a normal variable declaration is that we don't care what
							// the generated value is for a constrained variable.  These are
							// the "foo"s in "foo = func();" statements, for example.  We
							// implement constrained vars the same was as normal vars, just
							// using a single-state generator.
							let ty = type_from_decl(&decltype, &self.typetab);
							let gen = variable::generator_single(&ty);
							let sym = Symbol{name: nm.clone(), generator: gen,
							                 typ: ty.clone()};
							self.symtab.push(sym);
						},
						Declaration::Function(_) => (),
						Declaration::UDT(_) => (),
					};
				},
				_ => (),
			};
		}
	}

	// Ensures there is a type for every declaration.
	fn populate_typetable(&mut self) {
		for ref decl in self.declarations.iter() {
			match **decl {
				Declaration::UDT(ref udt) => {
					let typ = type_from_decl(&udt, &self.typetab);
					self.typetab.push(typ);
				},
				Declaration::Constrained(_, _) => (),
				Declaration::Free(_) => (),
				Declaration::Function(_) => (),
			};
		}
		for ref stmt in self.ast.iter() {
			match **stmt {
				Stmt::Declaration(ref decltype) => {
					match *decltype {
						Declaration::Constrained(_, ref decl) => {
							let typ = type_from_decl(&decl, &self.typetab);
							self.typetab.push(typ.clone());
						},
						Declaration::Free(ref fvd) => {
							let typ = type_from_decl(&fvd.ty, &self.typetab);
							self.typetab.push(typ.clone());
						},
						Declaration::Function(_) => (), // right?
						Declaration::UDT(_) => (), // right?
					}
				},
				_ => (),
			};
		}
	}

	// We have two types of expressions: "AST" expressions and stmt::Expressions.
	// The former are string based; the latter reference symbols from our
	// self.symtab.  This converts from the AST variation to the analyzed version.
	fn expr_to_expr(&self, expr: Expr) -> stmt::Expression {
		match expr {
			Expr::VarRef(ref sop, ref nm) => {
				let v = self.symlookup(nm).unwrap();
				stmt::Expression::Basic(*sop, v.clone())
			},
			Expr::IConst(iger) => {
				use std::str::FromStr;
				stmt::Expression::IConstant(i64::from_str(&iger).unwrap())
			},
			Expr::FConst(fp) => {
				use std::str::FromStr;
				stmt::Expression::FConstant(f64::from_str(&fp).unwrap())
			},
			Expr::Call(ref nm, ref arglist) => {
				let mut args: Vec<function::Argument> = Vec::new();
				use std::ops::Deref;
				for a in arglist.deref().iter() {
					let ex = self.expr_to_expr(a.clone());
					args.push(function::Argument::new(&ex));
				}
				let symfunc = self.symlookup(nm).unwrap();
				let rettype = symfunc.typ.clone();
				let fqn = function::Function::new(&nm, &rettype, &args);
				stmt::Expression::FqnCall(fqn)
			},
			Expr::Compound(ref l, ref bop, ref r) => {
				use std::ops::Deref;
				let lhs = self.expr_to_expr(l.deref().clone());
				let rhs = self.expr_to_expr(r.deref().clone());
				stmt::Expression::Compound(Box::new(lhs), bop.clone(), Box::new(rhs))
			},
			Expr::Field(symname, fld) => {
				let var = self.symlookup(&symname).unwrap();
				stmt::Expression::Field(var.clone(), fld)
			},
		}
	}

	// We have two kinds of statements: "AST" statements and stmt::Statements.
	// The former is the unanalyzed result of the parser.  The latter is the
	// post-analysis result that references our internal data structures.  This
	// does the analysis to turn the former into the latter.
	fn stmt_to_stmt(&self, s: Stmt) -> Option<stmt::Statement> {
		match s {
			Stmt::Basic(ref expr) => {
				match *expr {
					Expr::VarRef(ref op, ref nm) => {
						println!("Statement with no effect: '{}{}'", op.to_string(), nm);
						None
					},
					Expr::IConst(ref i) => panic!("iconst {} cannot be a statement!", i),
					Expr::FConst(ref f) => panic!("fconst {} cannot be a statement!", f),
					Expr::Call(_, _) => {
						let exp = self.expr_to_expr(expr.clone());
						Some(stmt::Statement::Expr(exp))
					},
					Expr::Compound(_, ref op, _) => {
						println!("Compond statement ({}) with no effect.", op.to_string());
						None
					},
					Expr::Field(ref sym, ref fld) => {
						println!("Statement with no effect: '{}.{}'", sym, fld);
						None
					},
				}
			},
			Stmt::Declaration(ref decltype) => {
				match *decltype {
					Declaration::Constrained(ref nm, _) => {
						let sym = self.symlookup(&nm).unwrap();
						Some(stmt::Statement::VariableDeclaration(sym.name.clone(),
						                                          sym.typ.clone()))
					},
					Declaration::Free(ref fvd) => {
						let sym = self.symlookup(&fvd.name).unwrap();
						Some(stmt::Statement::VariableDeclaration(sym.name.clone(),
						                                          sym.typ.clone()))
					},
					Declaration::Function(_) => None, // right?
					Declaration::UDT(_) => None, // right ?
				}
			},
			Stmt::Assignment(ref lhs, ref rhs) => {
				let l = self.expr_to_expr(lhs.clone());
				let r = self.expr_to_expr(rhs.clone());
				Some(stmt::Statement::Assignment(l, r))
			},
			Stmt::Verify(ref expr) => {
				Some(stmt::Statement::Verify(self.expr_to_expr(expr.clone())))
			},
			Stmt::Constraint(ref expr) => {
				Some(stmt::Statement::Constraint(self.expr_to_expr(expr.clone())))
			}
			Stmt::If(ref expr, ref stmts) => {
				use std::ops::Deref;
				let mut statements: Vec<stmt::Statement> = vec![];
				for s in stmts.deref().iter() {
					let stopt = match self.stmt_to_stmt(s.clone()) {
						None => return None,
						Some(st) => st,
					};
					statements.push(stopt);
				}
				Some(stmt::Statement::If(self.expr_to_expr(expr.clone()),
				                         Box::new(statements)))
			},
			Stmt::While(ref expr, ref stmts) => {
				use std::ops::Deref;
				let mut statements: Vec<stmt::Statement> = vec![];
				for s in stmts.deref().iter() {
					let stopt = match self.stmt_to_stmt(s.clone()) {
						None => return None,
						Some(st) => st,
					};
					statements.push(stopt);
				}
				Some(stmt::Statement::While(self.expr_to_expr(expr.clone()),
				                            Box::new(statements)))
			}
		}
	}

	// Resolves references and the like in the AST.
	// After, the AST list will be empty and our list of Statements will have
	// everything we need.
	fn ast_resolve(&mut self) {
		let mut stmts: Vec<stmt::Statement> = Vec::with_capacity(self.ast.len());
		for var in self.symtab.iter() {
			// hack: we want to insert functions into our symtable so that we can
			// lookup the function's type from its name.  but we don't want to
			// declare functions as variables.
			// function entries in the symtable have bogus GenNothing generators
			// (because a generator makes no sense for a function call; it generates
			// whatever the call does!), so use that as a hack to see if it's
			// something we should be creating.
			if var.generator.name() == "std:nothing" {
				continue;
			}
			let s = stmt::Statement::VariableDeclaration(var.name.clone(),
			                                             var.typ.clone());
			stmts.push(s);
		}
		for stmt in self.ast.iter() {
			match self.stmt_to_stmt(stmt.clone()) {
				None => (),
				Some(s) => stmts.push(s),
			};
		}
		// declarations need to come first, so we add the existing statements to
		// what we just created instead of the other way around.
		stmts.append(&mut self.statements);
		self.statements = stmts;
		// Clear our AST.  This makes sure we get odd behavior if we try to use
		// this after analysis.
		self.ast.clear();
	}

	pub fn analyze(&mut self) -> Result<(),String> {
		self.populate_typetable();
		self.populate_symtable();
		self.ast_resolve();
		self.genlist.clear();
		Ok(())
	}

	pub fn prologue(&self, strm: &mut std::io::Write, headers: &Vec<&str>) ->
		std::io::Result<()> {
		try!(writeln!(strm, "#define _POSIX_C_SOURCE 201212L"));
		try!(writeln!(strm, "#define _GNU_SOURCE 1"));
		for h in headers.iter() {
			try!(writeln!(strm, "#include <{}>", h));
		}
		try!(write!(strm, "\n"));
		try!(writeln!(strm, "int main() {{"));
		return Ok(());
	}

	pub fn epilogue(&self, strm: &mut std::io::Write) -> std::io::Result<()> {
		try!(writeln!(strm, "\n\treturn 0;\n}}"));
		return Ok(());
	}

	pub fn codegen(&self, strm: &mut std::io::Write) ->
		Result<(),std::io::Error> {
		use stmt::Code;
		for stmt in self.statements.iter() {
			try!(write!(strm, "\t"));
			try!(stmt.codegen(strm, &self));
			try!(write!(strm, "\n"));
		}
		Ok(())
	}

	// We are done when all the generators for every symbol have reached their
	// end state.
	pub fn done(&self) -> bool {
		return self.symtab.iter().all(
			|ref sym| sym.generator.done()
		);
	}

	// Iterate. Move the most-appropriate generator to its next state.
	// precondition: !self.done()
	pub fn next(&mut self) {
		// find the last symbol which is not done.
		let nxt = match self.symtab.iter().rposition(|ref sym| {
			!sym.generator.done()
		}) {
			None => panic!("No next state?"),
			Some(idx) => idx,
		};
		assert!(!self.symtab[nxt].generator.done());
		// Iterate that 'last not-done symbol'.
		self.symtab[nxt].generator.next();

		// reset all subsequent symbols.
		for idx in nxt+1..self.symtab.len() {
			self.symtab[idx].generator.reset();
		}
	}

	// Counts the number of states this program represents.
	pub fn n_states(&self) -> usize {
		return self.symtab.iter().fold(1, |n: usize, ref sym| {
			return n*sym.generator.n_state();
		});
	}
}

// gives the type from the declaration.
// it needs to take the current type list as well, because this DeclType may
// reference other types, and it would need to produce boxes to those types.
fn type_from_decl(decl: &DeclType, types: &Vec<Type>) -> Type {
	match decl {
		&DeclType::Basic(ref ty) => ty.clone(),
		&DeclType::Struct(ref snm, ref flds) => {
			let mut flds_rv: Vec<(String, Box<Type>)> = Vec::new();
			for f in flds {
				match f.ty {
					DeclType::Basic(ref ty) =>
						flds_rv.push((f.name.clone(), Box::new(ty.clone()))),
					DeclType::Struct(_, _) => {
						// correct?
						let subtype = type_from_decl(&f.ty, types);
						flds_rv.push((f.name.clone(), Box::new(subtype)));
					},
					DeclType::Enum(_, _) => unreachable!(),
					DeclType::StructRef(ref nm) => {
						for t in types {
							match t {
								&Type::Struct(ref tgt, _) if *nm==*tgt => {
									flds_rv.push((f.name.clone(), Box::new(t.clone())));
									break;
								},
								_ => (),
							}
						}
					},
					DeclType::EnumRef(/*ref nm*/ _) => unimplemented!(),
				}
			}
			Type::Struct(snm.clone(), flds_rv)
		},
		&DeclType::Enum(ref enm, ref evalues) => {
			Type::Enum(enm.clone(), evalues.clone())
		},
		&DeclType::StructRef(ref nm) => {
			let mut rv: Type = Type::Builtin(Native::Void);
			for typex in types {
				match typex {
					&Type::Struct(ref strct, _) if strct == nm => rv = typex.clone(),
					_ => {},
				};
			}
			// Didn't find it?  Then bail, unknown type!
			if rv == Type::Builtin(Native::Void) {
				panic!("Unknown struct '{}'!", nm);
			}
			rv
		}
		&DeclType::EnumRef(ref nm) => {
			let mut rv: Type = Type::Builtin(Native::Void);
			for typex in types {
				match typex {
					&Type::Enum(ref enm, _) if enm == nm => rv = typex.clone(),
					&Type::Enum(ref enm, _) => {
						println!("Enum '{}' is not a match for '{}'", enm, nm);
					}
					_ => {},
				};
			}
			// Didn't find it?  Then bail, unknown type!
			if rv == Type::Builtin(Native::Void) {
				panic!("Unknown enum '{}'!", nm);
			}
			rv
		},
	}
}

#[cfg(test)]
mod test {
	use api;
	use fuzz;
	use typ::{Native, Type};

	#[test]
	fn empty_struct() {
		let s = "struct entry { }";
		assert!(fuzz::parse_LDeclarations(s).is_ok());
		assert_eq!(fuzz::parse_LDeclarations(s).unwrap().len(), 1);
		let ref decl: api::Declaration = fuzz::parse_LDeclarations(s).unwrap()[0];
		let decl = match decl {
			&api::Declaration::UDT(ref udt) => udt,
			_ => panic!("invalid declaration parse {:?}", decl),
		};
		use api::DeclType;
		match decl {
			&DeclType::Basic(_) => panic!("type should be Struct, is Basic"),
			&DeclType::Enum(_, _) => panic!("type should be Struct, is Enum"),
			&DeclType::EnumRef(_) => panic!("type should be Struct, is EnumRef"),
			&DeclType::StructRef(_) => panic!("type should be Struct, is StructRef"),
			&DeclType::Struct(ref nm, ref decllist) => {
				assert_eq!(*nm, "entry".to_string());
				assert_eq!(decllist.len(), 0)
			},
		};
	}

	#[test]
	fn struct_pointer_char() {
		let s = "struct Ent { pointer char key; }";
		assert!(fuzz::parse_LDeclarations(s).is_ok());
		assert_eq!(fuzz::parse_LDeclarations(s).unwrap().len(), 1);
		let ref decl: api::Declaration = fuzz::parse_LDeclarations(s).unwrap()[0];
		let decl = match decl {
			&api::Declaration::UDT(ref udt) => udt,
			_ => panic!("invalid declaration parse {:?}", decl),
		};
		use api::DeclType;
		match decl {
			&DeclType::Basic(_) => panic!("type should be UDT, is Basic"),
			&DeclType::Enum(_, _) => panic!("type should be UDT, is Enum"),
			&DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			&DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			&DeclType::Struct(ref nm, ref decllist) => {
				assert_eq!(*nm, "Ent".to_string());
				assert_eq!(decllist.len(), 1);
				let ref key: api::UDTDecl = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					api::DeclType::Struct(_, _) => panic!("incorrect type UDT for 'key'"),
					api::DeclType::Enum(_, _) => panic!("incorrect type Enum for 'key'"),
					api::DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					api::DeclType::Basic(ref blt) => {
						let ch = Type::Builtin(Native::Character);
						assert_eq!(blt, &Type::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}

	#[test]
	fn struct_multiple_fields() {
		let s = "struct Entry {\n".to_string() +
			"pointer char key;\n" +
			"pointer void value;\n" +
		"}";
		assert!(fuzz::parse_LDeclarations(s.as_str()).is_ok());
		assert_eq!(fuzz::parse_LDeclarations(s.as_str()).unwrap().len(), 1);
		let ref decl: api::Declaration =
			fuzz::parse_LDeclarations(s.as_str()).unwrap()[0];
		let decl = match decl {
			&api::Declaration::UDT(ref udt) => udt,
			_ => panic!("invalid declaration parse {:?}", decl),
		};
		use api::DeclType;
		match decl {
			&DeclType::Basic(_) => panic!("type should be UDT, is Basic"),
			&DeclType::Enum(_, _) => panic!("type should be UDT, is Enum"),
			&DeclType::EnumRef(_) => panic!("type should be UDT, is EnumRef"),
			&DeclType::StructRef(_) => panic!("type should be UDT, is StructRef"),
			&DeclType::Struct(ref nm, ref decllist) => {
				assert_eq!(*nm, "Entry".to_string());
				assert_eq!(decllist.len(), 2);
				let ref key: api::UDTDecl = decllist[0];
				assert_eq!(key.name, "key");
				match key.ty {
					DeclType::Struct(_, _) => panic!("incorrect type UDT for 'key'"),
					DeclType::Enum(_, _) => panic!("incorrect type Enum for 'key'"),
					DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					DeclType::Basic(ref blt) => {
						let ch = Type::Builtin(Native::Character);
						assert_eq!(blt, &Type::Pointer(Box::new(ch)));
					}
				}
				let ref value: api::UDTDecl = decllist[1];
				assert_eq!(value.name, "value");
				match value.ty {
					DeclType::Struct(_, _) => panic!("incorrect type UDT for 'key'"),
					DeclType::Enum(_, _) => panic!("incorrect type Enum for 'key'"),
					DeclType::EnumRef(_) => panic!("incorrect type for 'key'"),
					DeclType::StructRef(_) => panic!("incorrect type for 'key'"),
					DeclType::Basic(ref blt) => {
						let ch = Type::Builtin(Native::Void);
						assert_eq!(blt, &Type::Pointer(Box::new(ch)));
					}
				}
			},
		};
	}

	#[test]
	fn enum_single() {
		let s = "enum Enumeration { BLAH = 0 , }";
		match fuzz::parse_LDeclarations(s) {
			Ok(_) => {},
			Err(e) => panic!("{:?}", e),
		};
		let t = "enum Enumeration { BLA = 0 , }";
		assert!(fuzz::parse_LDeclarations(t).is_ok());
		assert_eq!(fuzz::parse_LDeclarations(t).unwrap().len(), 1);
	}

	#[test]
	fn enum_multi() {
		let s = "enum Enumeration { FOO = 0 , BAR = 1 , BAZ = 42 , }";
		let decls = match fuzz::parse_LDeclarations(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
	}

	#[test]
	fn struct_fvar_single() {
		let s = "struct X { } var:free blah gen:I32 i32";
		let decls = match fuzz::parse_LDeclarations(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 2);
	}

	#[test]
	fn parse_function_new() {
		let s = "function:new hcreate_r int {usize, pointer struct hsearch_data,}";
		let decls: Vec<api::Declaration> = match fuzz::parse_LDeclarations(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
		let fqn = match decls[0] {
			api::Declaration::Function(ref f) => f,
			_ => panic!("non function type {:?}", decls[0]),
		};
		assert_eq!(fqn.name, "hcreate_r");
		match fqn.retval {
			api::DeclType::Basic(ref ty) => match ty {
				&Type::Builtin(ref t) => assert_eq!(*t, Native::Integer),
				_ => panic!("basic type, but {:?}, not integer", ty),
			},
			_ => panic!("retval should be a basic type, not {:?}", fqn.retval),
		};
		assert_eq!(fqn.arguments.len(), 2);
		match fqn.arguments[0] {
			api::DeclType::Basic(ref ty) => match ty {
				&Type::Builtin(ref t) => assert_eq!(*t, Native::Usize),
				_ => panic!("basic type, but {:?} not usize", ty),
			},
			_ => panic!("arg0 should be a basic type, not {:?}", fqn.arguments[0]),
		};
		let ptr: &Type = match fqn.arguments[1] {
			api::DeclType::Basic(ref ptr) => ptr,
			_ => panic!("invalid arg1: {:?}", fqn.arguments[1]),
		};
		let boxptr = match ptr {
			&Type::Pointer(ref b) => b,
			_ => panic!("invalid ptr type {:?}", ptr),
		};
		use std::ops::Deref;
		match boxptr.deref() {
			&Type::Struct(ref nm, _) => assert_eq!(nm, "hsearch_data"),
			_ => panic!("invalid box ptr {:?}", boxptr),
		};
	}

	#[test]
	fn parse_two_function_decls() {
		let s = "function:new hcreate_r int {".to_string() +
			"usize, pointer struct hsearch_data," +
		"}" +
		"function:new hsearch_r int {" +
			"int, int, pointer pointer int, pointer struct hsearch_data," +
		"}";
		let decls: Vec<api::Declaration> =
			match fuzz::parse_LDeclarations(s.as_str()) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 2);
		let fqn = match decls[0] {
			api::Declaration::Function(ref f) => f,
			_ => panic!("non function type {:?}", decls[0]),
		};
		assert_eq!(fqn.name, "hcreate_r");
	}

	#[test]
	fn opaque_struct_in_function() {
		let s = "struct hsearch_data {}\n".to_string() +
		"var:free tbl gen:opaque struct hsearch_data\n" +
		"function:new hcreate_r int {" +
			"usize, pointer struct hsearch_data,\n" +
		"}\n";
		let decls: Vec<api::Declaration> =
			match fuzz::parse_LDeclarations(s.as_str()) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 3);
		// should assert that the hcreate_r's 2nd arg == types[0].
	}

	#[test]
	fn compound_expr() {
		let s = "var:free x gen:std:I32 i32\n".to_string() +
			"var:free y gen:std:I32 i32\n" +
			"constraint:new x > 0 && y < 0\n";
		let mut pgm: api::Program = match fuzz::parse_LProgram(s.as_str()) {
			Ok(p) => p,
			Err(e) => panic!("{:?}", e),
		};
		match pgm.analyze() { Err(e) => panic!(e), Ok(_) => () };
	}

	#[test]
	fn field_expr() {
		let s = "struct Entry {\n".to_string() +
				"pointer char key;\n" +
				"pointer void value;\n" +
			"}\n" +
			"var:free e gen:opaque struct Entry\n" +
			"e.value = 0\n";
		let mut pgm: api::Program = match fuzz::parse_LProgram(s.as_str()) {
			Ok(p) => p,
			Err(e) => panic!("{:?}", e),
		};
		match pgm.analyze() { Err(e) => panic!(e), Ok(_) => () };
	}
}
