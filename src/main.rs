use std::{env, fs};

fn main() {
    let input =
        fs::read_to_string(env::args().nth(1).expect("no filename")).expect("failed to read file");
    let (cst, interner) = refl::parser::parse_cst(&input);

    println!(
        "{}",
        refl::syntax::SyntaxNode::new_root(cst).debug(&interner, true)
    );
}
