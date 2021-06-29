use std::convert::TryInto;

#[derive(Debug)]
pub struct Grid {
    size: usize,
    edge_top: EdgeRow,
    edge_bottom: EdgeRow,
    edge_left: EdgeRow,
    edge_right: EdgeRow,
    guesses : Vec<bool>,
    guess_balls : usize,
    possible_solution : Vec<bool>,
    solution_ball_count : usize,
}

#[derive(PartialEq, Clone, Copy, Debug)]
struct BeamId(u8);

type EdgeRow = Vec<Deflection>;

#[derive(PartialEq, Clone, Copy, Debug)]
enum Deflection {
    EmptyRow,
    EmptyCol,
    HeadOn,
    Reflect,
    Through(BeamId),
}

enum Side {
    Top = 0,
    Left = 1,
    Right = 2,
    Bottom = 3,
}

#[derive(PartialEq, Clone, Copy)]
enum BallType {
    Solution,
    Guess,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum GridError {
    BadCoordinates,
    DuplicateBall,
    TooBig,
}

#[derive(Debug, PartialEq)]
pub struct Point(pub usize, pub usize);

#[derive(Debug)]
pub enum ParsePointError {
    ParseIntError(std::num::ParseIntError),
    ImproperFormat,
}

impl std::convert::From<std::num::ParseIntError> for ParsePointError {
    fn from(error: std::num::ParseIntError) -> Self {
        ParsePointError::ParseIntError(error)
    }
}

impl std::str::FromStr for Point {
    type Err = ParsePointError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // format: 2 unsigned integers separated by a space
        let coords: Vec<&str> = s.trim().split(' ').collect();
        if coords.len() != 2 {
            return Err(ParsePointError::ImproperFormat);
        }
        let x = coords[0].parse::<usize>()?;
        let y = coords[0].parse::<usize>()?;
        Ok(Point(x, y))
    }
}

struct Coord(isize, isize);

impl std::ops::Add<Coord> for Coord {
    type Output = Coord;

    fn add(self, rhs: Coord) -> Coord {
        Coord(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl std::convert::From<Point> for Coord {
    fn from(item: Point) -> Self {
        Coord(item.0 as isize, item.1 as isize)
    }
}

impl std::convert::TryFrom<Coord> for Point {
    type Error = ();
    fn try_from(item: Coord) -> Result<Self, Self::Error> {
        if item.0 < 0 || item.1 < 0 {
            Err(())
        } else {
            Ok(Point(item.0 as usize, item.1 as usize))
        }
    }
}

const UP : Coord = Coord(-1, 0);
const DOWN : Coord = Coord(1, 0);
const LEFT : Coord = Coord(0, -1);
const RIGHT : Coord = Coord(0, 1);

impl Grid {

    const MAX_SIZE : usize = 20;

    pub fn new(size: usize) -> Result<Grid, GridError> {
        if size > Grid::MAX_SIZE {
            return Err(GridError::TooBig)
        }
        Ok(Grid {
            size,
            edge_top: vec![Deflection::EmptyRow; size],
            edge_bottom: vec![Deflection::EmptyRow; size],
            edge_left: vec![Deflection::EmptyCol; size],
            edge_right: vec![Deflection::EmptyCol; size],
            guesses: vec![false; size * size],
            possible_solution: vec![false; size * size],
            guess_balls: 0,
            solution_ball_count: 0,
        })
    }

    fn add_ball(&mut self, p: Point, t: BallType) -> Result<(), GridError> {
        let i = self.point_to_index(p).ok_or(GridError::BadCoordinates)?;
        let (v,c) = match t {
            BallType::Solution => (&mut self.possible_solution, &mut self.solution_ball_count),
            BallType::Guess => (&mut self.guesses, &mut self.guess_balls),
        };
        if v[i] {
            return Err(GridError::DuplicateBall);
        }
        v[i] = true;
        *c += 1;
        Ok(())
    }

    pub fn add_ball_guess(&mut self, p: Point) -> Result<(), GridError> {
        self.add_ball(p, BallType::Guess)
    }

    pub fn add_ball_solution(&mut self, p: Point) -> Result<(), GridError> {
        self.add_ball(p, BallType::Solution)
    }

    pub fn fmt_solution(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_grid(BallType::Solution, f)
    }

    pub fn fmt_guess(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_grid(BallType::Guess, f)
    }

    fn edge_step(&self, start: Coord, t: BallType) -> Result<Coord, GridError> {
        let dir = match start {
            // top edge
            Coord(-1, _) => DOWN,
            // left edge
            Coord(_, -1) => RIGHT,
            // right edge
            Coord(_, y) if y == self.size as isize => LEFT,
            // bottom edge
            Coord(x, _) if x == self.size as isize => UP,
            // unknown edge
            _ => return Err(GridError::BadCoordinates),
        };
        // check cell in front
        if self.is_ball((start + dir).try_into().or(Err(GridError::BadCoordinates))?, t).ok_or(GridError::BadCoordinates)? {
            // head on
        }
        Ok(Coord(0,0))
    }

    fn next_step(&self, start: Point, dir: Coord, t: BallType) {

    }

    fn fmt_grid(&self, t: BallType, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_edge_row(&self.edge_top, f)?;
        for row in 0..self.size {
            self.format_row(row, t, f)?;
        }
        format_edge_row(&self.edge_bottom, f)
    }

    fn point_to_index(&self, p: Point) -> Option<usize> {
        if p.0 < self.size && p.1 < self.size {
            Some(p.0 * self.size + p.1)
        } else {
            None
        }
    }

    fn is_ball(&self, p: Point, t: BallType) -> Option<bool> {
        let index = self.point_to_index(p)?;
        match t {
            BallType::Guess => Some(self.guesses[index]),
            BallType::Solution => Some(self.possible_solution[index]),
        }
    }

    fn format_row(&self, row: usize, t: BallType, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.edge_left[row])?;
        for cell in 0..self.size {
            if self.is_ball(Point(row, cell), t) == Some(true) {
                write!(f, "O")?;
            } else {
                write!(f, ".")?;
            }
        }
        write!(f, "{}\n", self.edge_right[row])
    }
}

fn format_edge_row(row: &EdgeRow, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "+")?;
    for cell in row {
        write!(f, "{}", cell)?
    }
    write!(f, "+\n")
}

impl std::fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_grid(BallType::Guess, f)
    }
}

impl std::fmt::Display for BeamId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            0..=9 => write!(f, "{}", self.0),
            10..=32 => write!(f, "{}", ('a' as u8 + self.0 - 10) as char),
            _ => write!(f, "U"),
        }
    }
}

impl std::fmt::Display for Deflection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Deflection::HeadOn => write!(f, "H"),
            Deflection::Reflect => write!(f, "R"),
            Deflection::Through(v) => write!(f, "{}", v),
            Deflection::EmptyRow => write!(f, "-"),
            Deflection::EmptyCol => write!(f, "|"),
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use std::convert::TryFrom;

    #[test]
    fn coord_into_point() {
        assert_eq!(Point::try_from(Coord(2,3)), Ok(Point(2,3)));
        assert_eq!(Point::try_from(Coord(0,0)), Ok(Point(0,0)));
        assert_eq!(Point::try_from(Coord(0,2)), Ok(Point(0,2)));
        assert_eq!(Point::try_from(Coord(1,0)), Ok(Point(1,0)));
        assert_eq!(Point::try_from(Coord(-1,3)), Err(()));
        assert_eq!(Point::try_from(Coord(1,-2)), Err(()));
        assert_eq!(Point::try_from(Coord(-3,-4)), Err(()));
    }

    #[test]
    fn point_into_coord() {

    }

    #[test]
    fn parse_point(){

    }

    #[test]
    fn ball_adding_guess() {

    }

    #[test]
    fn ball_adding_solution() {

    }


}