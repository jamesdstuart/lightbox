use lightbox::{Point, Puzzle};
use std::io;
use std::io::prelude::*;

fn main() {
    let mut p = Puzzle::new(5).unwrap();
    print!("{}", p);
    p.add_ball_guess(Point(2, 3)).unwrap();
    print!("{}", p);

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let input = line.unwrap();
        println!("{}", input);
        let i: Point = input.parse().unwrap();
        p.add_ball_guess(i).unwrap();
        println!("{}", p);
    }
}
