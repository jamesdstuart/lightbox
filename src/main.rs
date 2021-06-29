use blackbox::{Grid, Point};
use std::io;
use std::io::prelude::*;

fn main() {
    let mut g = Grid::new(5).unwrap();
    print!("{}", g);
    g.add_ball_guess(Point(2,3)).unwrap();
    print!("{}", g);

    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let input = line.unwrap();
        println!("{}", input);
        let p : Point = input.parse().unwrap();
        g.add_ball_guess(p).unwrap();
        println!("{}", g);
    }
}
