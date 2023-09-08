pub mod hir;
pub mod parser;
pub mod span;
pub mod syntax;

pub fn compile(input: &str) {
    let (cst, interner) = parser::parse_cst(&input);

    println!(
        "{}",
        syntax::SyntaxNode::new_root(cst).debug(&interner, true)
    );
}
