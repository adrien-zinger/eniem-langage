use crate::tree::{self, *};

enum ENode {
	Init(Init),
	Assign(Assignation),
	Copy,
	/// Function call
	Call(Call),
	Delete,
	Scope(Scope),
	AfterScope,
	/// Function declaration
	Function(Function),
	Empty
}

impl ENode {
	fn blocking(&self) -> bool {
		match self {
			ENode::Init(n) => n.blocking,
			_ => false
		}
	}

	fn push(&mut self, c: ENode) {
		match self {
			ENode::Init(n) => n.childs.push(c),
			_ => ()
		}
	}
}

/// public access to interpreter
pub fn run(input: &Compound) {
	compound(input);
}

fn statement(input: &Statement) -> ENode {
	match &input.inner {
		EStatement::Function(input) => ENode::Function(function(input)),
		EStatement::Str(text) => todo!("allocate variable with text"),
		EStatement::Compound(input) => ENode::Scope(compound(input)),
		EStatement::Copy(var) => todo!("create copy of variable content"),
		EStatement::Ref(var) => todo!("get reference of variable"),
		EStatement::Call(input) => ENode::Call(call(input)),
	}
}

/// Node that doesn't produce jobs..
struct Function {
	/// Arguments tags (need an assignment)
	arguments: Vec<String>,
	scope: Box<Scope>,
}

/// Create an abstract function node that doesn't has to produce jobs. 
fn function(input: &tree::Function) -> Function {
	Function {
		arguments: input.args.clone(),
		scope: Box::new(compound(&input.inner))
	}
}

/// Subnode that will use a function declaration to work.
struct Call {
}

fn call(input: &tree::Call) -> Call {
	todo!()
}

// If I see a compound:
// { let a = "abc"; let b = a; b; }
// 
// Create a new scope that will be referenced in each subjobs.
// Each Job that will be created in that subtree will increase the scope length.
// When a Job in that subtree resolved, I decrease the atomic counter 'length' of the scope.
//
// The Job that assign the last expression to a "return box" is counted in the length.
// When the counter reach 0, I create a "Delete" Job for each variable declarated in the scope excepted
// for the return box.

struct Scope {
	len: usize,
	/// Child nodes of the scope.
	childs: Vec<ENode>,
	/// Boxes's tags to delete.
	to_delete: Vec<String>,
}



/// Recursive way to create a tree from a serie of expression.
fn expressions(mut node: ENode, exprs: &[Expression]) -> (ENode, Vec<String>) {
	let mut index = 0;
	let mut decls = vec![];
	for expr in exprs {
		index = index + 1;
		let mut child = match &expr.inner {
			EExpression::Statement(input) => statement(input),
			EExpression::Assignation(input) => ENode::Assign(assignation(input)),
			EExpression::Declaration(input) => {
				let init = declaration(input);
				let decl = init.tag.clone();
				decls.push(decl);
				ENode::Init(init)
			}
		};

		if child.blocking() {
			let (child, mut sub_decls) = expressions(child, &exprs[index..]);
			decls.append(&mut sub_decls);
			node.push(child);
			break;
		}

		node.push(child);
	}

	(node, decls)
}

fn compound(input: &Compound) -> Scope {
	let scope = Scope {
		len: input.inner.len(),
		childs: vec![],
		to_delete: vec![]
	};

	if let (ENode::Scope(mut scope), mut decls) = expressions(ENode::Scope(scope), &input.inner) {
		scope.to_delete.append(&mut decls);
		return scope;
	} else {
		unreachable!()
	}
}

// A) let a = b;
//  1. I build a Job that check if a box named 'b' is initialized.
//  2. I build a child Job that initialize a box 'a' that is a new reference to 'b'.
// B) let a = { ... }; (or statement to resolve in right part)

struct Assignation {
	/// Is current node marked as "block_on"
	blocking: bool,
}

fn assignation(input: &tree::Assignation) -> Assignation {
	todo!()
}

struct Init {
	/// Is current node marked as "block_on"
	blocking: bool,
	/// New tag
	tag: String,
	/// Right Part that I need to initialize the new box.
	right: Box<ENode>,
	/// Childs (things that require current job to resolve first)
	childs: Vec<ENode>
}

fn declaration(input: &tree::Assignation) -> Init {
	Init {
		blocking: input.block_on,
		tag: input.var.clone(),
		childs: vec![].into(),
		right: Box::new(statement(&input.to_assign))
	}
}
