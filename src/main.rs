use std::{env, fs};

fn main() {
    let input =
        fs::read_to_string(env::args().nth(1).expect("no filename")).expect("failed to read file");

    refl::compile(&input);
}
