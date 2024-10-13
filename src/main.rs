use std::env;
use std::fs;
use std::process::ExitCode;

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

    match command.as_str() {
        "tokenize" => {
            let file_contents = fs::read_to_string(filename).unwrap_or_else(|_| {
                eprintln!("Failed to read file {}", filename);
                String::new()
            });
            
            for token in Lexer::new(file_contents.chars()) {
                match token {
                    Ok(token) => println!("{}", token),
                    Err(e) => {
                        eprintln!("[line 1]: Error: {}", e);
                        exit_code = 65;
                    }
                }
            }
            println!("EOF  null"); // Placeholder, remove this line when implementing the scanner
        }
        _ => {
            eprintln!("Unknown command: {}", command);
            return;
        }
    }
    
    std::process::exit(exit_code)
}
