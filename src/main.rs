use std::env;
use std::fs;

use codecrafters_interpreter::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        eprintln!("Usage: {} tokenize <filename>", args[0]);
        return;
    }

    let command = &args[1];
    let filename = &args[2];

    let mut exit_code = 0;

    let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
        eprintln!("Failed to read file {}", filename);
        String::new()
    });

    match command.as_str() {
        "tokenize" => {
            for token in lex::Lexer::new(&file_contents) {
                match token {
                    Ok(token) => println!("{token}"),
                    Err(e) => {
                        eprintln!("{e}");
                        exit_code = 65;
                    }
                }
            }

            println!("EOF  null");
        }
        "parse" => {
            let ast = parse::Parser::new(&file_contents).parse();

            match ast {
                Ok(ast) => {
                    for stmt in ast {
                        println!("{stmt}");
                    }
                }
                Err(e) => {
                    eprintln!("{e}");
                    exit_code = 65;
                }
            }
        }
        "evaluate" => {
            let ast = parse::Parser::new(&file_contents).parse();

            match ast {
                Ok(ast) => match eval::Evaluator::new(ast).evaluate() {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("{}", e.error);
                        exit_code = 70;
                    }
                },
                Err(e) => {
                    eprintln!("{}", e);
                    exit_code = 65;
                }
            }
        }
        "run" => {
            let ast = parse::Parser::new(&file_contents).parse();

            match ast {
                Ok(ast) => match eval::Evaluator::new(ast).evaluate() {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("{}", e.error);
                        exit_code = 70;
                    }
                },
                Err(e) => {
                    eprintln!("{}", e);
                    exit_code = 65;
                }
            }
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }

    std::process::exit(exit_code)
}
