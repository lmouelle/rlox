pub mod scanner;
pub mod parser;

use std::{process::exit, env};

fn repl() {
    let mut buf = String::new();
    let res = std::io::stdin().read_line(&mut buf);
    match res {
        Ok(0) => exit(0),
        Ok(_) => todo!(),
        Err(foo) => eprintln!("{}", foo)
    }
}

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() == 1 {
        repl()
    } else if args.len() == 2 {
        // TODO: run files
    } else {
        eprintln!("usage: rlox [path]");
        exit(127);
    }
}
