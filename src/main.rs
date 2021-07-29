use lightbox::{Point, Puzzle};
use std::io;
use std::io::prelude::*;
extern crate clap;
use clap::{value_t, App, Arg};

fn prompt() {
    print!("> ");
    io::stdout().flush().unwrap();
}

fn print_help() {
    // note the extra indents are for the repeated braces
    println!(
        r#"
Commands:
    [add] {{row}} {{col}}               add a new ball guess
    remove {{row}} {{col}}              remove a ball guess
    check                           check guess against solution
    clear                           clear all guesses
    help                            print this command help message

    row and col are 0-indexed
    "#
    );
}

fn parse_point(s: &str) -> Option<Point> {
    match s.parse::<Point>() {
        Ok(p) => Some(p),
        Err(e) => {
            eprintln!(
                "Couldn't parse a point from \"{}\". Expected format is \"row col\". Error: {:?}",
                s, e
            );
            None
        }
    }
}

const size_arg: &str = "grid_size";

fn main() {
    // TODO implement versioning
    let matches = App::new("Light Box Puzzle")
        .author("James Stuart <james.stuart@ieee.org>")
        .arg(Arg::with_name(size_arg).required(true))
        .get_matches();

    let size = value_t!(matches, size_arg, usize).unwrap_or_else(|e| e.exit());

    println!("Generating new puzzle of size {size}x{size}\n", size = size);
    let mut puzzle = match Puzzle::new(size) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Couldn't create new puzzle: {:?}", e);
            std::process::exit(1);
        }
    };

    println!("{}", puzzle);
    print_help();
    prompt();

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let input = line.unwrap();
        let input = input.trim();
        if input.len() == 0 {
        } else if input.starts_with("remove ") {
            if let Some(p) = parse_point(&input["remove ".len()..]) {
                match puzzle.remove_ball_guess(p) {
                    Ok(_) => {}
                    Err(e) => eprintln!("Couldn't remove ball: {:?}", e),
                }
            }
        } else if input == "check" {
            match puzzle.check_guess() {
                Ok(true) => {
                    println!("Congratulations! Puzzle solved!\n");
                    println!("For reference, we had the following solution (which may be different, but is equivalent)\n{}", puzzle.get_solution());
                    break;
                }
                Ok(false) => {
                    println!("Sorry, you are incorrect.\n");
                }
                Err(e) => eprintln!("I'm very sorry - an error occurred. {:?}", e),
            };
        } else if input == "clear" {
            puzzle.clear_guess();
        } else if input == "give up" {
            println!("Weakling!\n");
            println!("{}", puzzle.get_solution());
        } else if input.starts_with("add ") {
            if let Some(p) = parse_point(&input["add ".len()..]) {
                match puzzle.add_ball_guess(p) {
                    Ok(_) => {}
                    Err(e) => eprintln!("Couldn't add ball: {:?}", e),
                }
            }
        } else if input.chars().nth(0).unwrap().is_ascii_digit() {
            if let Some(p) = parse_point(input) {
                match puzzle.add_ball_guess(p) {
                    Ok(_) => {}
                    Err(e) => eprintln!("Couldn't add ball: {:?}", e),
                }
            }
        } else if input == "exit" || input == "quit" || input == "q" {
            break;
        } else {
            println!("Couldn't understand given command.");
            print_help();
        }
        println!("\n{}", puzzle);
        prompt();
    }
}
