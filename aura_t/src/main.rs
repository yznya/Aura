use std::env;

use at::Interpreter;

fn main() {
    let args: Vec<_> = env::args().collect();

    let mut interpreter = Interpreter { had_error: false };
    if args.len() == 2 {
        interpreter.run_file(&args[1])
    } else if args.len() == 1 {
        interpreter.prompt();
        interpreter.had_error = false;
    } else {
        println!("Usage: at [script]");
    }
}
