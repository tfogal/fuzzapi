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

impl DeclType {
	fn typename(&self) -> String {
		use typ::Name;
		match self {
			&DeclType::Basic(ref ty) => ty.name(),
			&DeclType::Struct(ref nm, _) => nm.clone(),
			&DeclType::Enum(ref nm, _) => nm.clone(),
			&DeclType::StructRef(ref to) => to.clone(),
			&DeclType::EnumRef(ref to) => to.clone(),
		}
	}
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
	Free(FreeVarDecl),
	Function(FuncDecl),
	UDT(DeclType), // Error if the DeclType is not a Struct || Enum!
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

#[derive(Debug)]
pub struct Program {
	pub declarations: Vec<Declaration>,
	pub statements: Vec<stmt::Statement>,
	symtab: Vec<Symbol>,
	typetab: Vec<Type>,
	// copy of generator list.  Expected users will clone() out of it to create
	// the real/used Generators (that live in the symbol table).
	genlist: Vec<Box<variable::Generator>>,
}

impl Program {
	pub fn new(decls: &Vec<Declaration>, stmts: &Vec<stmt::Statement>)
		-> Program {
		Program{declarations: (*decls).clone(), statements: (*stmts).clone(),
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
		for gen in self.genlist.iter() {
			if gen.name() == genname {
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
				Declaration::Function(_) => (),
				Declaration::UDT(_) => (),
			}
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
				Declaration::Free(_) => (),
				Declaration::Function(_) => (),
			}
		}
	}

	// Our parsing is a bit weird in that it generates both declarations and then
	// a list of statements.  Undo the weirdness by converting all the
	// declarations into VarDecl statements, so that we can then just codegen()
	// all the statements without worrying it'll break stuff.
	// We should really just fix our parser to generate Declaration Statements in
	// the first place...
	fn insert_declarations(&mut self) {
		// first a quick error check. this method converts declarations into
		// statements. thus it should only be called once, else you'll end up with
		// duplicate declarations. Make sure there are no VarDecls in our target.
		for stmt in self.statements.iter() {
			match stmt {
				&stmt::Statement::VariableDeclaration(ref nm, ref typ) => {
					panic!("VarDecl for {:?}:{:?} already in statements!", nm, typ);
				},
				_ => (),
			};
		}

		let mut stmts: Vec<stmt::Statement> = Vec::with_capacity(self.symtab.len());
		for var in self.symtab.iter() {
			let s = stmt::Statement::VariableDeclaration(var.name.clone(),
			                                             var.typ.clone());
			stmts.push(s);
		}
		// declarations need to come first, so we add the existing statements to
		// what we just created instead of the other way around.
		stmts.append(&mut self.statements);
		self.statements = stmts;
	}

	pub fn analyze(&mut self) -> Result<(),String> {
		self.populate_typetable();
		self.populate_symtable();
		self.insert_declarations();
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
					&Type::Struct(ref strct, _) => {
						println!("struct '{}' is not a match for '{}'", strct, nm);
					}
					_ => {},
				};
			}
			/* Didn't find it?  Then bail, unknown type! */
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
			/* Didn't find it?  Then bail, unknown type! */
			if rv == Type::Builtin(Native::Void) {
				panic!("Unknown enum '{}'!", nm);
			}
			rv
		},
	}
}

fn func_from_decl(fqn: &FuncDecl, types: &Vec<Type>,
                  gen: &Vec<Box<variable::Generator>>) -> function::Function {
	let rtype = type_from_decl(&fqn.retval, &types);
	let fauxsrc = variable::Source::free("???", &rtype, "std:nothing", &gen);
	let retv = function::ReturnType::new(&rtype, fauxsrc);
	let mut rv = function::Function{
		name: fqn.name.clone(),
		arguments: Vec::new(),
		retval: retv
	};
	for arg in fqn.arguments.iter() {
		let typedecl: Type = type_from_decl(&arg, &types);
		let src = variable::Source::free("???", &typedecl, "std:nothing", &gen);
		rv.arguments.push(function::Argument::new(&typedecl, src));
	}
	return rv;
}

// replaces the "Decl" types from this module with the typ::* counterparts,
// potentially panic'ing due to invalid semantics.
pub fn resolve_types(decls: &Vec<Declaration>,
                     generators: &mut Vec<Box<variable::Generator>>) ->
	(Vec<Type>, Vec<variable::Source>) {
	assert!(decls.len() > 0);
	let mut drv: Vec<Type> = Vec::new();
	let mut vars: Vec<variable::Source> = Vec::new();

	for decl in decls {
		match decl {
			&Declaration::Free(ref fr) => {
				let gname: String = if fr.genname == "opaque" {
					"std:opaque:".to_string() + &fr.ty.typename()
				} else if fr.genname == "udt" || fr.genname == "UDT" {
					"std:udt:".to_string() + &fr.ty.typename()
				} else if fr.genname == "enum" || fr.genname == "Enum" {
					"std:enum:".to_string() + &fr.ty.typename()
				} else {
					fr.genname.clone()
				};
				let typedecl: Type = type_from_decl(&fr.ty, &drv);
				let fvar = variable::Source::free(&fr.name, &typedecl, &gname,
				                                  generators);
				vars.push(fvar);
			},
			&Declaration::Function(ref fqn) => {
				let func = func_from_decl(fqn, &drv, generators);
				drv.push(Type::Function(Box::new(func)));
			},
			&Declaration::UDT(ref udecl) => {
				let typedecl: Type = type_from_decl(&udecl, &drv);
				drv.push(typedecl.clone());
				match udecl {
					&DeclType::Struct(ref x, _) => {
						let opaque = variable::GenOpaque::create(&typedecl);
						generators.push(Box::new(opaque));
						x
					},
					&DeclType::Enum(ref x, _) => {
						let genenum = variable::GenEnum::create(&typedecl);
						generators.push(Box::new(genenum));
						x
					},
					_ => panic!("invalid DeclType {:?} for UDT", udecl),
				};
			},
		};
	}
	(drv, vec![])
}

#[cfg(test)]
mod test {
	use api;
	use fuzz;
	use typ::{Native, Type};
	use variable;

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
		let mut gens: Vec<Box<variable::Generator>> = Vec::new();
		let (decl, _) =
			api::resolve_types(&fuzz::parse_LDeclarations(s).unwrap(), &mut gens);
		assert_eq!(decl.len(), 1);
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
	fn func_from_decl() {
		let declint = api::DeclType::Basic(Type::Builtin(Native::Integer));
		let declszt = api::DeclType::Basic(Type::Builtin(Native::Usize));
		let fd = api::FuncDecl{
			name: "hcreate".to_string(),
			retval: declint,
			arguments: vec![declszt]
		};
		let generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let typelist: Vec<Type> = Vec::new();
		api::func_from_decl(&fd, &typelist, &generators);
	}

	#[test]
	fn type_resolution() {
		let s = "struct Entry {\n".to_string() +
			"pointer char key;\n" +
			"pointer void value;\n" +
		"}\n" +
		"var:free tbl gen:opaque struct Entry";
		let decls: Vec<api::Declaration> =
			match fuzz::parse_LDeclarations(s.as_str()) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let (types, srcs) = api::resolve_types(&decls, &mut generators);
		assert_eq!(types.len(), 1);
		assert_eq!(srcs.len(), 0);
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
		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let (types, _) = api::resolve_types(&decls, &mut generators);
		assert_eq!(generators.len(), 2);
		match types[0] {
			Type::Struct(ref nm, ref fields) => {
				assert_eq!(nm, "hsearch_data");
				assert_eq!(fields.len(), 0);
			}
			_ => panic!("first type ({:?}) should be struct hsearch_data", types[0]),
		};
		// should assert that the hcreate_r's 2nd arg == types[0].
	}

	#[test]
	fn enum_resolves_with_generator() {
		let s = "enum ACTION { ENTER=0, FIND=1, }\n";
		let decls: Vec<api::Declaration> = match fuzz::parse_LDeclarations(s) {
			Ok(parsed) => parsed,
			Err(e) => panic!("{:?}", e),
		};
		assert_eq!(decls.len(), 1);
		let mut generators: Vec<Box<variable::Generator>> =
			vec![Box::new(variable::GenNothing{})];
		let (types, _) = api::resolve_types(&decls, &mut generators);
		assert_eq!(generators.len(), 2);
		assert_eq!(types.len(), 1);
		match types[0] {
			Type::Enum(ref enm, _) => assert_eq!(enm, "ACTION"),
			_ => (),
		};
	}
}
