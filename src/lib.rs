use std::convert::TryFrom;
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
    SomethingWentWrong,
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

struct Edges {
    size: usize,
    count: usize,
}

impl Edges {
    fn new(size: usize) -> Edges {
        Edges { size, count: 0 }
    }
}

impl Iterator for Edges {
    type Item = Coord;
    fn next(&mut self) -> Option<Self::Item> {
        let i = (self.count % self.size) as isize;
        let r = match self.count {
            // top edge
            c if c < 1 * self.size => Some(Coord(-1, i)),
            // left edge
            c if c < 2 * self.size => Some(Coord(i, -1)),
            // right edge
            c if c < 3 * self.size => Some(Coord(i, self.size as isize)),
            // bottom edge
            c if c < 4 * self.size => Some(Coord(self.size as isize, i)),
            _ => None,
        };
        if r != None {
            self.count += 1;
        }
        return r;
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

    fn get_side_coords(&self, front: Coord) -> (Coord, Coord) {
        let (dirLeft, dirRight) = self.get_sides();
        (front + dirLeft, front + dirRight)
    }

    fn reflect(self) -> Direction {
        match self {
            Direction::UP => Direction::DOWN,
            Direction::LEFT => Direction::RIGHT,
            Direction::RIGHT => Direction::LEFT,
            Direction::DOWN => Direction::UP,
            _ => Coord(0, 0),
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

    fn edges(&self) -> Edges {
        Edges::new(self.size)
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

    fn at_edge(&self, c: Coord) -> bool {
        match c {
            Coord(-1, _) | Coord(_, -1) => true,
            Coord(_, y) if y == self.size as isize => true,
            Coord(x, _) if x == self.size as isize => true,
            _ => false,
        }
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
        let (side_left, side_right) = dir.get_sides();
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
        let cell_left = start + dir + side_left;
        if self.coord_inside_grid(&cell_left)
            && self.is_ball(cell_left.try_into().or(Err(GridError::BadCoordinates))?, t)
                == Some(true)
        {
            return Ok(Beam::Reflect);
        }
        // check cell right
        // there may not be one, so check if cell exists and a ball is there
        let cell_right = start + dir + side_right;
        if self.coord_inside_grid(&cell_right)
            && self.is_ball(cell_right.try_into().or(Err(GridError::BadCoordinates))?, t)
                == Some(true)
        {
            return Ok(Beam::Reflect);
        }
        Ok(Beam::Through(start + dir, dir))
    }

    fn next_step(&self, start: Point, dir: Coord, t: BallType) -> Result<Beam, GridError> {
        let start: Coord = start.into();
        // check front
        let front = start + dir;

        // if we're at an edge, we've passed through
        if self.at_edge(front) {
            return Ok(Beam::Edge(front, dir));
        }
        // if there is a ball
        if self
            .is_ball(front.try_into().or(Err(GridError::BadCoordinates))?, t)
            .ok_or(GridError::BadCoordinates)?
        {
            // return head on
            return Ok(Beam::HeadOn);
        }

        // check left and right for reflection
        let (side_left, side_right) = dir.get_sides();
        let (left, right) = dir.get_side_coords(front);
        let left_edge = self.at_edge(left);
        let right_edge = self.at_edge(right);

        let left_ball = if !left_edge {
            self.is_ball(left.try_into().or(Err(GridError::BadCoordinates))?, t)
                .ok_or(GridError::BadCoordinates)?
        } else {
            false
        };

        let right_ball = if !right_edge {
            self.is_ball(right.try_into().or(Err(GridError::BadCoordinates))?, t)
                .ok_or(GridError::BadCoordinates)?
        } else {
            false
        };

        // if both, reflect
        if left_ball && right_ball {
            // return swapped direction
            return Ok(Beam::Through(start, dir.reflect()));
        }

        // check left
        if left_ball {
            // turn right
            return Ok(Beam::Through(start, side_right));
        }

        if right_ball {
            return Ok(Beam::Through(start, side_left));
        }

        // continue 1 step forward
        Ok(Beam::Through(start + dir, dir))
    }

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

    fn get_edge(&self, edge: Coord) -> Result<Deflection, GridError> {
        if !self.at_edge(edge) {
            return Err(GridError::BadCoordinates);
        }
        let (edge_row, index) = match edge {
            Coord(-1, i) => (&self.edge_top, i),
            // left edge
            Coord(i, -1) => (&self.edge_left, i),
            // right edge
            Coord(i, y) if y == self.size as isize => (&self.edge_right, i),
            // bottom edge
            Coord(x, i) if x == self.size as isize => (&self.edge_bottom, i),
            _ => return Err(GridError::BadCoordinates),
        };
        if index < 0 || index > self.size as isize {
            return Err(GridError::BadCoordinates);
        };
        let index: usize = index as usize;
        Ok(edge_row[index])
    }

    fn set_edge(&mut self, edge: Coord, value: Deflection) -> Result<(), GridError> {
        if !self.at_edge(edge) {
            return Err(GridError::BadCoordinates);
        }
        let (edge_row, index) = match edge {
            Coord(-1, i) => (&mut self.edge_top, i),
            // left edge
            Coord(i, -1) => (&mut self.edge_left, i),
            // right edge
            Coord(i, y) if y == self.size as isize => (&mut self.edge_right, i),
            // bottom edge
            Coord(x, i) if x == self.size as isize => (&mut self.edge_bottom, i),
            _ => return Err(GridError::BadCoordinates),
        };
        if index < 0 || index > self.size as isize {
            return Err(GridError::BadCoordinates);
        };
        let index: usize = index as usize;
        edge_row[index] = value;
        Ok(())
    }

    fn walk_from_edge(&mut self, start: Coord, t: BallType) -> Result<Beam, GridError> {
        let r = self.edge_step(start, t)?;
        let (mut next, mut dir) = match r {
            Beam::HeadOn => return Ok(Beam::HeadOn),
            Beam::Reflect => return Ok(Beam::Reflect),
            Beam::Edge(_, _) => return Err(GridError::SomethingWentWrong),
            Beam::Through(n, d) => (n, d),
        };
        loop {
            let r = self.next_step(next.try_into().or(Err(GridError::BadCoordinates))?, dir, t)?;
            match r {
                Beam::HeadOn => return Ok(r),
                Beam::Edge(e, _) => {
                    if e == start {
                        return Ok(Beam::Reflect);
                    } else {
                        return Ok(r);
                    }
                }
                Beam::Reflect => return Err(GridError::SomethingWentWrong),
                Beam::Through(n, d) => {
                    next = n;
                    dir = d;
                }
            };
        }

        // Shouldn't get to this point
        // Err(GridError::SomethingWentWrong)
    }

    fn generate_edges(&mut self) -> Result<(), GridError> {
        let t = BallType::Solution;
        let mut count = 1;
        // loop through edges
        let edges = self.edges();
        for edge in edges {
            // check if already a through
            match self.get_edge(edge)? {
                Deflection::Through(_) => continue,
                Deflection::EmptyRow | Deflection::EmptyCol => {}
                _ => return Err(GridError::SomethingWentWrong),
            };

            let r = self.walk_from_edge(edge, t)?;
            match r {
                Beam::HeadOn => self.set_edge(edge, Deflection::HeadOn)?,
                Beam::Reflect => self.set_edge(edge, Deflection::Reflect)?,
                Beam::Edge(other_edge, _) => {
                    if other_edge == edge {
                        self.set_edge(edge, Deflection::Reflect)?;
                    } else {
                        self.set_edge(edge, Deflection::Through(BeamId(count)))?;
                        self.set_edge(other_edge, Deflection::Through(BeamId(count)))?;
                        count += 1;
                    }
                }
                _ => return Err(GridError::SomethingWentWrong),
            };
        }
        Ok(())
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
        assert_eq!(Coord::from(Point(0, 0)), Coord(0, 0));
        assert_eq!(Coord::from(Point(0, 2)), Coord(0, 2));
        assert_eq!(Coord::from(Point(3, 0)), Coord(3, 0));
        assert_eq!(Coord::from(Point(5, 7)), Coord(5, 7));
    }

    #[test]
    fn parse_point() {
        assert_eq!(Ok(Point(1, 2)), Point::from_str("1 2"));
        assert_eq!(Err(ParsePointError::ImproperFormat), Point::from_str("1"));
        assert_eq!(Err(ParsePointError::ImproperFormat), Point::from_str("1,2"));
        match Point::from_str("a b") {
            Err(ParsePointError::ParseIntError(_)) => {}
            x => panic!("expected ParseIntError, got {:?}", x),
        }
        match Point::from_str("1 b") {
            Err(ParsePointError::ParseIntError(_)) => {}
            x => panic!("expected ParseIntError, got {:?}", x),
        }
        match Point::from_str("a 2") {
            Err(ParsePointError::ParseIntError(_)) => {}
            x => panic!("expected ParseIntError, got {:?}", x),
        }
    }

    #[test]
    fn edge_set_get() {
        let s = 3;
        let mut g = Grid::new(s as usize).unwrap();
        for index in 0..s {
            let edge = Coord(-1, index);
            assert_eq!(Ok(Deflection::EmptyRow), g.get_edge(edge));
            let edge = Coord(s, index);
            assert_eq!(Ok(Deflection::EmptyRow), g.get_edge(edge));
            let edge = Coord(index, -1);
            assert_eq!(Ok(Deflection::EmptyCol), g.get_edge(edge));
            let edge = Coord(index, s);
            assert_eq!(Ok(Deflection::EmptyCol), g.get_edge(edge));
        }

        assert_eq!(Ok(()), g.set_edge(Coord(-1, 0), Deflection::HeadOn));
        assert_eq!(Ok(()), g.set_edge(Coord(-1, 1), Deflection::Reflect));
        assert_eq!(
            Ok(()),
            g.set_edge(Coord(-1, 2), Deflection::Through(BeamId(1)))
        );

        assert_eq!(Ok(()), g.set_edge(Coord(s, 0), Deflection::HeadOn));
        assert_eq!(Ok(()), g.set_edge(Coord(s, 1), Deflection::Reflect));
        assert_eq!(
            Ok(()),
            g.set_edge(Coord(s, 2), Deflection::Through(BeamId(2)))
        );

        assert_eq!(Ok(()), g.set_edge(Coord(0, -1), Deflection::HeadOn));
        assert_eq!(Ok(()), g.set_edge(Coord(1, -1), Deflection::Reflect));
        assert_eq!(
            Ok(()),
            g.set_edge(Coord(2, -1), Deflection::Through(BeamId(1)))
        );

        assert_eq!(Ok(()), g.set_edge(Coord(0, s), Deflection::HeadOn));
        assert_eq!(Ok(()), g.set_edge(Coord(1, s), Deflection::Reflect));
        assert_eq!(
            Ok(()),
            g.set_edge(Coord(2, s), Deflection::Through(BeamId(1)))
        );

        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 0)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(-1, 1)));
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(-1, 2)));

        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(s, 0),));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(s, 1),));
        assert_eq!(Ok(Deflection::Through(BeamId(2))), g.get_edge(Coord(s, 2),));

        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(0, -1)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(1, -1)));
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(2, -1)));

        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(0, s),));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(1, s),));
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(2, s),));
    }

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

    #[test]
    fn next_step_1() {
        // define a 3x3 grid, with 2 balls, and expected edges
        //       R   H   R
        //     +-----------+
        //  H  |   | O |   | H
        //  1  |   |   |   | R
        //  H  |   |   | O | H
        //     +-----------+
        //       1   R   H

        let mut g = Grid::new(3).unwrap();
        g.add_ball_solution(Point(0, 1)).unwrap();
        g.add_ball_solution(Point(2, 2)).unwrap();

        // start top, left cell.
        assert_eq!(
            Ok(Beam::HeadOn),
            g.next_step(Point(0, 0), Direction::RIGHT, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(-1, 0), Direction::UP)),
            g.next_step(Point(0, 0), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::DOWN)),
            g.next_step(Point(0, 0), Direction::DOWN, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(0, -1), Direction::LEFT)),
            g.next_step(Point(0, 0), Direction::LEFT, BallType::Solution)
        );

        // left, middle cell
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::DOWN)),
            g.next_step(Point(1, 0), Direction::RIGHT, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::LEFT)),
            g.next_step(Point(1, 0), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::DOWN)),
            g.next_step(Point(1, 0), Direction::DOWN, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(1, -1), Direction::LEFT)),
            g.next_step(Point(1, 0), Direction::LEFT, BallType::Solution)
        );

        // left bottom cell
        assert_eq!(
            Ok(Beam::Through(Coord(2, 1), Direction::RIGHT)),
            g.next_step(Point(2, 0), Direction::RIGHT, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::UP)),
            g.next_step(Point(2, 0), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(3, 0), Direction::DOWN)),
            g.next_step(Point(2, 0), Direction::DOWN, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(2, -1), Direction::LEFT)),
            g.next_step(Point(2, 0), Direction::LEFT, BallType::Solution)
        );

        // middle centre cell
        // left and right directions aren't specified - shouldn't get into that situation
        assert_eq!(
            Ok(Beam::HeadOn),
            g.next_step(Point(1, 1), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 1), Direction::LEFT)),
            g.next_step(Point(1, 1), Direction::DOWN, BallType::Solution)
        );

        // centre bottom cell
        assert_eq!(
            Ok(Beam::HeadOn),
            g.next_step(Point(2, 1), Direction::RIGHT, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 1), Direction::UP)),
            g.next_step(Point(2, 1), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(3, 1), Direction::DOWN)),
            g.next_step(Point(2, 1), Direction::DOWN, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::LEFT)),
            g.next_step(Point(2, 1), Direction::LEFT, BallType::Solution)
        );

        // right top cell
        assert_eq!(
            Ok(Beam::Edge(Coord(0, 3), Direction::RIGHT)),
            g.next_step(Point(0, 2), Direction::RIGHT, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(-1, 2), Direction::UP)),
            g.next_step(Point(0, 2), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 2), Direction::DOWN)),
            g.next_step(Point(0, 2), Direction::DOWN, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.next_step(Point(0, 2), Direction::LEFT, BallType::Solution)
        );

        // right middle cell
        // left and right directions aren't specified - shouldn't get into that situation
        assert_eq!(
            Ok(Beam::Through(Coord(1, 2), Direction::RIGHT)),
            g.next_step(Point(1, 2), Direction::UP, BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.next_step(Point(1, 2), Direction::DOWN, BallType::Solution)
        );
    }

    #[test]
    fn edges() {
        let s = 4;
        let mut edges = Edges::new(s as usize);
        assert_eq!(Some(Coord(-1, 0)), edges.next());
        assert_eq!(Some(Coord(-1, 1)), edges.next());
        assert_eq!(Some(Coord(-1, 2)), edges.next());
        assert_eq!(Some(Coord(-1, 3)), edges.next());
        assert_eq!(Some(Coord(0, -1)), edges.next());
        assert_eq!(Some(Coord(1, -1)), edges.next());
        assert_eq!(Some(Coord(2, -1)), edges.next());
        assert_eq!(Some(Coord(3, -1)), edges.next());
        assert_eq!(Some(Coord(0, s)), edges.next());
        assert_eq!(Some(Coord(1, s)), edges.next());
        assert_eq!(Some(Coord(2, s)), edges.next());
        assert_eq!(Some(Coord(3, s)), edges.next());
        assert_eq!(Some(Coord(s, 0)), edges.next());
        assert_eq!(Some(Coord(s, 1)), edges.next());
        assert_eq!(Some(Coord(s, 2)), edges.next());
        assert_eq!(Some(Coord(s, 3)), edges.next());
        assert_eq!(None, edges.next());
        assert_eq!(None, edges.next());
    }

    #[test]
    fn edges_from_grid() {
        let s = 3;
        let g = Grid::new(s as usize).unwrap();
        let mut edges = g.edges();
        assert_eq!(Some(Coord(-1, 0)), edges.next());
        assert_eq!(Some(Coord(-1, 1)), edges.next());
        assert_eq!(Some(Coord(-1, 2)), edges.next());
        assert_eq!(Some(Coord(0, -1)), edges.next());
        assert_eq!(Some(Coord(1, -1)), edges.next());
        assert_eq!(Some(Coord(2, -1)), edges.next());
        assert_eq!(Some(Coord(0, s)), edges.next());
        assert_eq!(Some(Coord(1, s)), edges.next());
        assert_eq!(Some(Coord(2, s)), edges.next());
        assert_eq!(Some(Coord(s, 0)), edges.next());
        assert_eq!(Some(Coord(s, 1)), edges.next());
        assert_eq!(Some(Coord(s, 2)), edges.next());
        assert_eq!(None, edges.next());
        assert_eq!(None, edges.next());
    }

    #[test]
    fn walk_edges_1() {
        // define a 3x3 grid, with 2 balls, and expected edges
        //       R   H   R
        //     +-----------+
        //  H  |   | O |   | H
        //  1  |   |   |   | R
        //  H  |   |   | O | H
        //     +-----------+
        //       1   R   H

        let mut g = Grid::new(3).unwrap();
        g.add_ball_solution(Point(0, 1)).unwrap();
        g.add_ball_solution(Point(2, 2)).unwrap();

        // Top edge
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(-1, 0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(-1, 1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(-1, 2), BallType::Solution)
        );

        // Left edge
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(0, -1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(3, 0), Direction::DOWN)),
            g.walk_from_edge(Coord(1, -1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(2, -1), BallType::Solution)
        );

        // Right edge
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(0, 3), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(1, 3), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(2, 3), BallType::Solution)
        );

        // Bottom edge
        assert_eq!(
            Ok(Beam::Edge(Coord(1, -1), Direction::LEFT)),
            g.walk_from_edge(Coord(3, 0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(3, 1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(3, 2), BallType::Solution)
        );
    }

    #[test]
    fn walk_edges_2() {
        // define a 3x3 grid, with 2 balls, and expected edges
        //       H   R   H
        //     +-----------+
        //  H  | O |   | O | H
        //  R  |   |   |   | R
        //  1  |   |   |   | 1
        //     +-----------+
        //       H   R   H

        let mut g = Grid::new(3).unwrap();
        g.add_ball_solution(Point(0, 0)).unwrap();
        g.add_ball_solution(Point(0, 2)).unwrap();

        // Top edge
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(-1, 0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(-1, 1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(-1, 2), BallType::Solution)
        );

        // Left edge
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(0, -1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(1, -1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(2, 3), Direction::RIGHT)),
            g.walk_from_edge(Coord(2, -1), BallType::Solution)
        );

        // Right edge
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(0, 3), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(1, 3), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(2, -1), Direction::LEFT)),
            g.walk_from_edge(Coord(2, 3), BallType::Solution)
        );

        // Bottom edge
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(3, 0), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::Reflect),
            g.walk_from_edge(Coord(3, 1), BallType::Solution)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            g.walk_from_edge(Coord(3, 2), BallType::Solution)
        );
    }

    #[test]
    fn check_solution_edges_3x_2_1() {
        // define a 3x3 grid, with 2 balls, and expected edges
        //       R   H   R
        //     +-----------+
        //  H  |   | O |   | H
        //  1  |   |   |   | R
        //  H  |   |   | O | H
        //     +-----------+
        //       1   R   H

        let mut g = Grid::new(3).unwrap();
        g.add_ball_solution(Point(0, 1)).unwrap();
        g.add_ball_solution(Point(2, 2)).unwrap();
        g.generate_edges().unwrap();

        // Top edge
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(-1, 0)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 1)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(-1, 2)));

        // Left edge
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(0, -1)));
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(1, -1)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(2, -1)));

        // Right edge
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(0, 3)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(1, 3)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(2, 3)));

        // Bottom edge
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(3, 0)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(3, 1)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(3, 2)));
    }

    #[test]
    fn check_solution_edges_3x_2_2() {
        // define a 3x3 grid, with 2 balls, and expected edges
        //       H   R   H
        //     +-----------+
        //  H  | O |   | O | H
        //  R  |   |   |   | R
        //  1  |   |   |   | 1
        //     +-----------+
        //       H   R   H

        let mut g = Grid::new(3).unwrap();
        g.add_ball_solution(Point(0, 0)).unwrap();
        g.add_ball_solution(Point(0, 2)).unwrap();

        g.generate_edges().unwrap();

        // Top edge
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 0)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(-1, 1)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 2)));

        // Left edge
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(0, -1)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(1, -1)));
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(2, -1)));

        // Right edge
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(0, 3)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(1, 3)));
        assert_eq!(Ok(Deflection::Through(BeamId(1))), g.get_edge(Coord(2, 3)));

        // Bottom edge
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(3, 0)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(3, 1)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(3, 2)));
    }

    #[test]
    fn check_solution_edges_10x_6_1() {
        // define a 10x10 grid, with 6 balls, and expected edges
        //       H6HH1H9ab8
        //      +----------+
        //    1 |..........| 9
        //    H |.....O....| H
        //    R |..........| a
        //    H |O.....O...| H
        //    R |..O.......| H
        //    2 |..........| b
        //    H |.....O.O..| h
        //    3 |..........| 7
        //    4 |..........| 4
        //    5 |..........| 5
        //      +----------+
        //       H2H63HRH78

        let mut g = Grid::new(10).unwrap();
        g.add_ball_solution(Point(1, 5)).unwrap();
        g.add_ball_solution(Point(3, 0)).unwrap();
        g.add_ball_solution(Point(3, 6)).unwrap();
        g.add_ball_solution(Point(4, 2)).unwrap();
        g.add_ball_solution(Point(6, 5)).unwrap();
        g.add_ball_solution(Point(6, 7)).unwrap();
        g.generate_edges().unwrap();

        // Top edge, H & R
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 0)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 2)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 3)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(-1, 5)));
        // Left edge, H & R
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(1, -1)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(2, -1)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(3, -1)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(4, -1)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(6, -1)));
        // Right edge, H & R
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(1, 10)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(3, 10)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(4, 10)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(6, 10)));
        // Bottom edge, H & R
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(10, 0)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(10, 2)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(10, 5)));
        assert_eq!(Ok(Deflection::Reflect), g.get_edge(Coord(10, 6)));
        assert_eq!(Ok(Deflection::HeadOn), g.get_edge(Coord(10, 7)));

        // TODO: check for throughs in an ID agnostic manner
    }
}
