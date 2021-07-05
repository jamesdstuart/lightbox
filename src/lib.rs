use std::convert::TryInto;

#[derive(Debug)]
pub struct Grid {
    size: usize,
    edge_top: EdgeRow,
    edge_bottom: EdgeRow,
    edge_left: EdgeRow,
    edge_right: EdgeRow,
    guesses: Vec<bool>,
    guess_balls: usize,
    possible_solution: Vec<bool>,
    solution_ball_count: usize,
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

#[derive(Debug, PartialEq)]
enum Side {
    Top = 0,
    Left = 1,
    Right = 2,
    Bottom = 3,
}

#[derive(Debug, PartialEq)]
enum Beam {
    HeadOn,
    Edge(Coord, Direction),
    Through(Coord, Direction),
    Reflect,
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Point(pub usize, pub usize);

#[derive(Debug, PartialEq)]
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
        let y = coords[1].parse::<usize>()?;
        Ok(Point(x, y))
    }
}

#[derive(PartialEq, Debug, Eq, Copy, Clone)]
struct Coord(isize, isize);

impl Coord {
    fn top(col: isize) -> Coord {
        Coord(-1, col)
    }
    fn left(row: isize) -> Coord {
        Coord(row, -1)
    }
}

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

type Direction = Coord;

impl Direction {
    const UP: Coord = Coord(-1, 0);
    const DOWN: Coord = Coord(1, 0);
    const LEFT: Coord = Coord(0, -1);
    const RIGHT: Coord = Coord(0, 1);

    fn get_sides(&self) -> (Direction, Direction) {
        match *self {
            Direction::LEFT => (Direction::DOWN, Direction::UP),
            Direction::RIGHT => (Direction::UP, Direction::DOWN),
            Direction::UP => (Direction::LEFT, Direction::RIGHT),
            Direction::DOWN => (Direction::RIGHT, Direction::LEFT),
            _ => (Coord(0, 0), Coord(0, 0)),
        }
    }
}

impl Grid {
    const MAX_SIZE: usize = 20;

    pub fn new(size: usize) -> Result<Grid, GridError> {
        if size > Grid::MAX_SIZE {
            return Err(GridError::TooBig);
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

    fn point_inside_grid(&self, p: &Point) -> bool {
        p.0 < self.size && p.1 < self.size
    }

    fn coord_inside_grid(&self, c: &Coord) -> bool {
        c.0 >= 0 && c.0 < self.size as isize && c.1 >= 0 && c.1 < self.size as isize
    }

    fn add_ball(&mut self, p: Point, t: BallType) -> Result<(), GridError> {
        let i = self.point_to_index(p).ok_or(GridError::BadCoordinates)?;
        let (v, c) = match t {
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

    fn edge_step(&self, start: Coord, t: BallType) -> Result<Beam, GridError> {
        let dir = match start {
            // top edge
            Coord(-1, _) => Direction::DOWN,
            // left edge
            Coord(_, -1) => Direction::RIGHT,
            // right edge
            Coord(_, y) if y == self.size as isize => Direction::LEFT,
            // bottom edge
            Coord(x, _) if x == self.size as isize => Direction::UP,
            // unknown edge
            _ => return Err(GridError::BadCoordinates),
        };
        let (sideLeft, sideRight) = dir.get_sides();
        // check cell in front
        // should always be a cell in front, error if not
        if self
            .is_ball(
                (start + dir)
                    .try_into()
                    .or(Err(GridError::BadCoordinates))?,
                t,
            )
            .ok_or(GridError::BadCoordinates)?
        {
            // head on
            return Ok(Beam::HeadOn);
        }
        // check cell left
        // there may not be one, so check if cell exists and a ball is there
        let cellLeft = start + dir + sideLeft;
        if self.coord_inside_grid(&cellLeft)
            && self.is_ball(cellLeft.try_into().or(Err(GridError::BadCoordinates))?, t)
                == Some(true)
        {
            return Ok(Beam::Reflect);
        }
        // check cell right
        // there may not be one, so check if cell exists and a ball is there
        let cellRight = start + dir + sideRight;
        if self.coord_inside_grid(&cellRight)
            && self.is_ball(cellRight.try_into().or(Err(GridError::BadCoordinates))?, t)
                == Some(true)
        {
            return Ok(Beam::Reflect);
        }
        Ok(Beam::Through(start + dir, dir))
    }

    fn next_step(&self, start: Point, dir: Coord, t: BallType) {}

    fn fmt_grid(&self, t: BallType, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_edge_row(&self.edge_top, f)?;
        for row in 0..self.size {
            self.format_row(row, t, f)?;
        }
        format_edge_row(&self.edge_bottom, f)
    }

    fn point_to_index(&self, p: Point) -> Option<usize> {
        if self.point_inside_grid(&p) {
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

    fn format_row(
        &self,
        row: usize,
        t: BallType,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
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
    use std::str::FromStr;

    #[test]
    fn coord_into_point() {
        assert_eq!(Point::try_from(Coord(2, 3)), Ok(Point(2, 3)));
        assert_eq!(Point::try_from(Coord(0, 0)), Ok(Point(0, 0)));
        assert_eq!(Point::try_from(Coord(0, 2)), Ok(Point(0, 2)));
        assert_eq!(Point::try_from(Coord(1, 0)), Ok(Point(1, 0)));
        assert_eq!(Point::try_from(Coord(-1, 3)), Err(()));
        assert_eq!(Point::try_from(Coord(1, -2)), Err(()));
        assert_eq!(Point::try_from(Coord(-3, -4)), Err(()));
    }

    #[test]
    fn point_into_coord() {
        assert_eq!(Coord::from(Point(0,0)), Coord(0,0));
        assert_eq!(Coord::from(Point(0,2)), Coord(0,2));
        assert_eq!(Coord::from(Point(3,0)), Coord(3,0));
        assert_eq!(Coord::from(Point(5,7)), Coord(5,7));
    }

    #[test]
    fn parse_point() {
        assert_eq!(Ok(Point(1,2)), Point::from_str("1 2"));
        assert_eq!(Err(ParsePointError::ImproperFormat), Point::from_str("1"));
        assert_eq!(Err(ParsePointError::ImproperFormat), Point::from_str("1,2"));
        // assert_eq!(Err(ParsePointError::ParseIntError(_)), Point::from_str("a b"));
        match Point::from_str("a b") {
            Err(ParsePointError::ParseIntError(_)) => {},
            x => panic!("expected ParseIntError, got {:?}", x),
        }
        match Point::from_str("1 b") {
            Err(ParsePointError::ParseIntError(_)) => {},
            x => panic!("expected ParseIntError, got {:?}", x),
        }
        match Point::from_str("a 2") {
            Err(ParsePointError::ParseIntError(_)) => {},
            x => panic!("expected ParseIntError, got {:?}", x),
        }
    }

    #[test]
    fn ball_adding_guess() {}

    #[test]
    fn ball_adding_solution() {}

    #[test]
    fn edge_step_1() {
        // define a 3x3 grid, with 2 balls, and expected edges
        //       R   H   R
        //     +-----------+
        //  H  |   | O |   | H
        //  1  |   |   |   | R
        //  H  |   |   | O | H
        //     +-----------+
        //       1   R   H
        // we test each of the 12 edge entry points, to ensure the correct next step is identified

        // grid drawn with just the *next step* along the edges
        //        R   H   R
        //      +-----------+
        //  T>  |   | O |   | T<
        //  T>  |   |   |   | R
        //  T>  |   |   | O | H
        //      +-----------+
        //        T^  R   H

        let mut g = Grid::new(3).unwrap();
        g.add_ball_solution(Point(0, 1)).unwrap();
        g.add_ball_solution(Point(2, 2)).unwrap();
        // go along top edge, left to right
        assert_eq!(
            Ok(Beam::Reflect),
            g.edge_step(Coord::top(0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.edge_step(Coord::top(1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.edge_step(Coord::top(2), BallType::Solution)
        );
        // left edge
        assert_eq!(
            Ok(Beam::Through(Coord(0, 0), Direction::RIGHT)),
            g.edge_step(Coord::left(0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::RIGHT)),
            g.edge_step(Coord::left(1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::RIGHT)),
            g.edge_step(Coord::left(2), BallType::Solution)
        );
        // right edge
        assert_eq!(
            Ok(Beam::Through(Coord(0, 2), Direction::LEFT)),
            g.edge_step(Coord(0, 3), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.edge_step(Coord(1, 3), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.edge_step(Coord(2, 3), BallType::Solution)
        );
        // bottom edge
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::UP)),
            g.edge_step(Coord(3, 0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.edge_step(Coord(3, 1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.edge_step(Coord(3, 2), BallType::Solution)
        );
    }
}
