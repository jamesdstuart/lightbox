use std::collections::HashSet;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::ops::Deref;

use rand::Rng;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct GridSize(usize);

impl From<GridSize> for usize {
    fn from(item: GridSize) -> usize {
        item.0
    }
}

impl From<GridSize> for isize {
    fn from(item: GridSize) -> isize {
        item.0 as isize
    }
}

impl From<&GridSize> for usize {
    fn from(item: &GridSize) -> usize {
        item.0
    }
}

impl TryFrom<usize> for GridSize {
    type Error = PuzzleError;
    fn try_from(value: usize) -> Result<Self, Self::Error> {
        GridSize::new(value).ok_or(PuzzleError::TooBigOrSmall)
    }
}

impl TryFrom<isize> for GridSize {
    type Error = PuzzleError;
    fn try_from(value: isize) -> Result<Self, Self::Error> {
        GridSize::new(value.try_into().or(Err(PuzzleError::TooBigOrSmall))?)
            .ok_or(PuzzleError::TooBigOrSmall)
    }
}

impl From<&GridSize> for isize {
    fn from(item: &GridSize) -> isize {
        item.0 as isize
    }
}

impl Deref for GridSize {
    type Target = usize;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl GridSize {
    fn new(size: usize) -> Option<GridSize> {
        if size > Puzzle::MAX_SIZE || size < Puzzle::MIN_SIZE {
            None
        } else {
            Some(GridSize(size))
        }
    }
    fn point_inside_grid(&self, p: &Point) -> bool {
        p.0 < self.0 && p.1 < self.0
    }

    fn point_to_index(&self, p: Point) -> Option<usize> {
        if self.point_inside_grid(&p) {
            Some(p.0 * self.0 + p.1)
        } else {
            None
        }
    }

    fn coord_inside_grid(&self, c: &Coord) -> bool {
        c.0 >= 0 && c.0 < self.into() && c.1 >= 0 && c.1 < self.into()
    }

    fn at_edge(&self, c: Coord) -> bool {
        match c {
            Coord(-1, _) | Coord(_, -1) => true,
            Coord(_, y) if y == self.into() => true,
            Coord(x, _) if x == self.into() => true,
            _ => false,
        }
    }

    fn points(&self) -> PointIter {
        PointIter::new(self.0)
    }
}

struct PointIter {
    size: usize,
    count: usize,
}

impl PointIter {
    fn new(size: usize) -> PointIter {
        PointIter { size, count: 0 }
    }
}

impl Iterator for PointIter {
    type Item = Point;
    fn next(&mut self) -> Option<Self::Item> {
        if self.count < self.size * self.size {
            let col = self.count % self.size;
            let row = self.count / self.size;
            self.count = self.count + 1;
            Some(Point(row, col))
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Puzzle {
    size: GridSize,
    edges: EdgeRows,
    guess: Balls,
    solution: Balls,
}

pub struct PuzzleSolution<'a> {
    puzzle: &'a Puzzle,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, PartialOrd, Ord)]
struct BeamId(u8);

type EdgeRow = Vec<Deflection>;

#[derive(Debug)]
struct EdgeRows {
    size: GridSize,
    top: EdgeRow,
    bottom: EdgeRow,
    left: EdgeRow,
    right: EdgeRow,
    through: HashSet<CoordPair>,
    through_count: BeamId,
}

#[derive(Debug, PartialEq)]
struct Balls {
    balls: Vec<bool>,
    size: GridSize,
    count: usize,
}

struct BallsIter {
    balls: Vec<Point>,
}

impl BallsIter {
    fn new(b: &Balls) -> BallsIter {
        let mut balls = Vec::with_capacity(b.count);
        for p in b.size.points() {
            match b.get(p) {
                Some(true) => balls.push(p),
                _ => continue,
            }
        }
        BallsIter { balls }
    }
}

impl Iterator for BallsIter {
    type Item = Point;
    fn next(&mut self) -> Option<Self::Item> {
        self.balls.pop()
    }
}

impl Balls {
    fn new(size: GridSize) -> Balls {
        Balls {
            size,
            balls: vec![false; size.0 * size.0],
            count: 0,
        }
    }

    fn add(&mut self, p: Point) -> Result<(), PuzzleError> {
        let i = self
            .size
            .point_to_index(p)
            .ok_or(PuzzleError::BadCoordinates)?;
        if self.balls[i] {
            return Err(PuzzleError::DuplicateBall);
        }
        self.balls[i] = true;
        self.count += 1;
        Ok(())
    }

    fn remove(&mut self, p: Point) -> Result<(), PuzzleError> {
        let i = self
            .size
            .point_to_index(p)
            .ok_or(PuzzleError::BadCoordinates)?;
        if !self.balls[i] {
            return Err(PuzzleError::NoSuchBall);
        }
        self.balls[i] = false;
        self.count -= 1;
        Ok(())
    }

    fn get(&self, p: Point) -> Option<bool> {
        Some(self.balls[self.size.point_to_index(p)?])
    }

    fn clear(&mut self) {
        for b in &mut self.balls {
            *b = false;
        }
        self.count = 0
    }

    fn iter(&self) -> BallsIter {
        BallsIter::new(&self)
    }
}

impl EdgeRows {
    fn new(size: GridSize) -> EdgeRows {
        EdgeRows {
            size,
            top: vec![Deflection::EmptyRow; size.into()],
            bottom: vec![Deflection::EmptyRow; size.into()],
            left: vec![Deflection::EmptyCol; size.into()],
            right: vec![Deflection::EmptyCol; size.into()],
            through: HashSet::with_capacity(usize::from(size) * 2),
            through_count: BeamId(0),
        }
    }

    fn edges(&self) -> Edges {
        Edges::new(self.size)
    }

    fn edges_top(&self) -> EdgesTop {
        EdgesTop::new(*self.size)
    }

    fn edges_left(&self) -> EdgesLeft {
        EdgesLeft::new(*self.size)
    }

    fn edges_right(&self) -> EdgesRight {
        EdgesRight::new(*self.size)
    }

    fn edges_bottom(&self) -> EdgesBottom {
        EdgesBottom::new(*self.size)
    }

    fn get_row_index(&self, edge: Coord) -> Option<(&EdgeRow, usize)> {
        let (edge_row, index) = match edge {
            Coord(-1, i) => (&self.top, i),
            // left edge
            Coord(i, -1) => (&self.left, i),
            // right edge
            Coord(i, y) if y == self.size.into() => (&self.right, i),
            // bottom edge
            Coord(x, i) if x == self.size.into() => (&self.bottom, i),
            _ => return None,
        };
        if index < 0 || index > self.size.into() {
            return None;
        };
        Some((edge_row, index as usize))
    }

    fn get_mut_row_index(&mut self, edge: Coord) -> Option<(&mut EdgeRow, usize)> {
        let (edge_row, index) = match edge {
            Coord(-1, i) => (&mut self.top, i),
            // left edge
            Coord(i, -1) => (&mut self.left, i),
            // right edge
            Coord(i, y) if y == self.size.into() => (&mut self.right, i),
            // bottom edge
            Coord(x, i) if x == self.size.into() => (&mut self.bottom, i),
            _ => return None,
        };
        if index < 0 || index > self.size.into() {
            return None;
        };
        Some((edge_row, index as usize))
    }

    fn get(&self, edge: Coord) -> Option<Deflection> {
        let (edge_row, index) = self.get_row_index(edge)?;
        Some(edge_row[index])
    }

    fn is_empty(&self, edge: Coord) -> Option<bool> {
        match self.get(edge)? {
            Deflection::EmptyCol | Deflection::EmptyRow => Some(true),
            _ => Some(false),
        }
    }

    fn add_head_on(&mut self, edge: Coord) -> Option<()> {
        // check it's blank first
        if !self.is_empty(edge)? {
            return None;
        }
        let (edge_row, index) = self.get_mut_row_index(edge)?;
        edge_row[index] = Deflection::HeadOn;
        Some(())
    }

    fn add_reflection(&mut self, edge: Coord) -> Option<()> {
        // check it's blank first
        if !self.is_empty(edge)? {
            return None;
        }
        let (edge_row, index) = self.get_mut_row_index(edge)?;
        edge_row[index] = Deflection::Reflect;
        Some(())
    }

    fn add_through(&mut self, edge1: Coord, edge2: Coord) -> Option<bool> {
        // check they are edges first
        if !self.size.at_edge(edge1) || !self.size.at_edge(edge2) {
            return None;
        }
        // check they are not equal
        if edge1 == edge2 {
            return None;
        }
        let pair = CoordPair(edge1, edge2);
        // insert pair, checking if already inserted
        if !self.through.insert(pair) {
            return Some(false);
        }
        // check both are blank
        if !self.is_empty(edge1)? || !self.is_empty(edge2)? {
            return None;
        }
        // add deflections to edges
        self.through_count.0 += 1;
        let id = self.through_count;
        let (edge_row, index) = self.get_mut_row_index(edge1)?;
        edge_row[index] = Deflection::Through(id);
        let (edge_row, index) = self.get_mut_row_index(edge2)?;
        edge_row[index] = Deflection::Through(id);
        Some(true)
    }

    fn equivalent_single_edge_row(this: &EdgeRow, other: &EdgeRow) -> bool {
        // check length
        if this.len() != other.len() {
            return false;
        }
        for (pos, d) in this.iter().enumerate() {
            if !d.equivalent(&other[pos]) {
                return false;
            }
        }
        true
    }

    fn equivalent_edges(&self, other: &Self) -> bool {
        EdgeRows::equivalent_single_edge_row(&self.top, &other.top)
            && EdgeRows::equivalent_single_edge_row(&self.left, &other.left)
            && EdgeRows::equivalent_single_edge_row(&self.right, &other.right)
            && EdgeRows::equivalent_single_edge_row(&self.bottom, &other.bottom)
    }

    fn are_empty_edges(&self) -> bool {
        for e in self.edges() {
            match self.get(e).unwrap() {
                Deflection::EmptyCol | Deflection::EmptyRow => return true,
                _ => {}
            }
        }
        return false;
    }
}

impl PartialEq for EdgeRows {
    fn eq(&self, other: &Self) -> bool {
        (self.size == other.size)
            && (self.through_count == other.through_count)
            && (self.through == other.through)
            && (self.equivalent_edges(other))
    }
}

impl Eq for EdgeRows {}

#[derive(Debug, Clone, Copy, Eq)]
struct CoordPair(Coord, Coord);

// A CoordPair is equal if it contains the same two Coord values, in any order.
impl PartialEq for CoordPair {
    fn eq(&self, other: &Self) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.0 == other.1 && self.1 == other.0)
    }
}

impl std::hash::Hash for CoordPair {
    fn hash<H: std::hash::Hasher>(&self, hasher: &mut H) {
        // because ordering of writes is important, need to have consistent ordering
        if self.0 <= self.1 {
            self.0.hash(hasher);
            self.1.hash(hasher);
        } else {
            self.1.hash(hasher);
            self.0.hash(hasher);
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Deflection {
    EmptyRow,
    EmptyCol,
    HeadOn,
    Reflect,
    Through(BeamId),
}

impl Deflection {
    fn equivalent(&self, other: &Self) -> bool {
        match self {
            Deflection::Through(x) => match other {
                Deflection::Through(y) => true,
                _ => false,
            },
            _ => self == other,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Side {
    Top = 0,
    Left = 1,
    Right = 2,
    Bottom = 3,
}

#[derive(Debug, PartialEq, Eq)]
enum Beam {
    HeadOn,
    Edge(Coord, Direction),
    Through(Coord, Direction),
    Reflect,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum BallType {
    Solution,
    Guess,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum PuzzleError {
    BadCoordinates,
    DuplicateBall,
    TooBigOrSmall,
    SomethingWentWrong,
    NoSuchBall,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Hash)]
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
    fn new(size: GridSize) -> Edges {
        Edges {
            size: size.0,
            count: 0,
        }
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

struct EdgesTop {
    size: usize,
    count: usize,
}

impl EdgesTop {
    fn new(size: usize) -> EdgesTop {
        EdgesTop { size, count: 0 }
    }
}

impl Iterator for EdgesTop {
    type Item = Coord;
    fn next(&mut self) -> Option<Self::Item> {
        let i = (self.count % self.size) as isize;
        let r = match self.count {
            // top edge
            c if c < 1 * self.size => Some(Coord(-1, i)),
            _ => None,
        };
        if r != None {
            self.count += 1;
        }
        return r;
    }
}

struct EdgesLeft {
    size: usize,
    count: usize,
}

impl EdgesLeft {
    fn new(size: usize) -> EdgesLeft {
        EdgesLeft { size, count: 0 }
    }
}

impl Iterator for EdgesLeft {
    type Item = Coord;
    fn next(&mut self) -> Option<Self::Item> {
        let i = (self.count % self.size) as isize;
        let r = match self.count {
            // left edge
            c if c < 1 * self.size => Some(Coord(i, -1)),
            _ => None,
        };
        if r != None {
            self.count += 1;
        }
        return r;
    }
}

struct EdgesRight {
    size: usize,
    count: usize,
}

impl EdgesRight {
    fn new(size: usize) -> EdgesRight {
        EdgesRight { size, count: 0 }
    }
}

impl Iterator for EdgesRight {
    type Item = Coord;
    fn next(&mut self) -> Option<Self::Item> {
        let i = (self.count % self.size) as isize;
        let r = match self.count {
            // right edge
            c if c < 1 * self.size => Some(Coord(i, self.size as isize)),
            _ => None,
        };
        if r != None {
            self.count += 1;
        }
        return r;
    }
}

struct EdgesBottom {
    size: usize,
    count: usize,
}

impl EdgesBottom {
    fn new(size: usize) -> EdgesBottom {
        EdgesBottom { size, count: 0 }
    }
}

impl Iterator for EdgesBottom {
    type Item = Coord;
    fn next(&mut self) -> Option<Self::Item> {
        let i = (self.count % self.size) as isize;
        let r = match self.count {
            // bottom edge
            c if c < 1 * self.size => Some(Coord(self.size as isize, i)),
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

impl Puzzle {
    const MAX_SIZE: usize = 20;
    const MIN_SIZE: usize = 3;

    pub fn new(size: usize) -> Result<Puzzle, PuzzleError> {
        let size = GridSize::new(size).ok_or(PuzzleError::TooBigOrSmall)?;
        let mut p = Puzzle {
            size,
            edges: EdgeRows::new(size),
            guess: Balls::new(size),
            solution: Balls::new(size),
        };
        p.generate_new_puzzle()?;
        Ok(p)
    }

    fn new_empty(size: usize) -> Result<Puzzle, PuzzleError> {
        let size = GridSize::new(size).ok_or(PuzzleError::TooBigOrSmall)?;
        Ok(Puzzle {
            size,
            edges: EdgeRows::new(size),
            guess: Balls::new(size),
            solution: Balls::new(size),
        })
    }

    fn generate_new_puzzle(&mut self) -> Result<(), PuzzleError> {
        let mut rng = rand::thread_rng();
        let min_balls = (self.size.0 / 2) + 1;
        let max_balls = self.size.0 + (self.size.0 / 2);
        let mut balls_to_add = rng.gen_range(min_balls..=max_balls);

        while balls_to_add > 0 {
            let p = Point(rng.gen_range(0..self.size.0), rng.gen_range(0..self.size.0));
            match self.add_ball_solution(p) {
                Ok(()) => balls_to_add = balls_to_add - 1,
                Err(PuzzleError::DuplicateBall) => {}
                Err(x) => return Err(x),
            };
        }
        self.generate_solution_edges()
    }

    fn edge_iter(&self) -> Edges {
        self.edges.edges()
    }

    fn point_inside_grid(&self, p: &Point) -> bool {
        self.size.point_inside_grid(p)
    }

    fn coord_inside_grid(&self, c: &Coord) -> bool {
        self.size.coord_inside_grid(c)
    }

    fn add_ball(&mut self, p: Point, t: BallType) -> Result<(), PuzzleError> {
        match t {
            BallType::Solution => self.solution.add(p),
            BallType::Guess => self.guess.add(p),
        }
    }

    pub fn add_ball_guess(&mut self, p: Point) -> Result<(), PuzzleError> {
        self.add_ball(p, BallType::Guess)
    }

    pub fn add_ball_solution(&mut self, p: Point) -> Result<(), PuzzleError> {
        self.add_ball(p, BallType::Solution)
    }

    pub fn remove_ball_guess(&mut self, p: Point) -> Result<(), PuzzleError> {
        self.guess.remove(p)
    }

    pub fn get_solution(&self) -> PuzzleSolution {
        PuzzleSolution { puzzle: &self }
    }

    fn fmt_solution(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_grid(BallType::Solution, f)
    }

    fn fmt_guess(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_grid(BallType::Guess, f)
    }

    pub fn clear_guess(&mut self) {
        self.guess.clear();
    }

    fn at_edge(&self, c: Coord) -> bool {
        self.size.at_edge(c)
    }

    fn fmt_grid(&self, t: BallType, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        format_edge_row(&self.edges.top, f)?;
        fmt_horizontal_edge(&self.size, f)?;
        for row in 0..self.size.into() {
            self.format_row(row, t, f)?;
        }
        fmt_horizontal_edge(&self.size, f)?;
        format_edge_row(&self.edges.bottom, f)
    }

    fn is_ball(&self, p: Point, t: BallType) -> Option<bool> {
        match t {
            BallType::Guess => self.guess.get(p),
            BallType::Solution => self.solution.get(p),
        }
    }

    fn format_row(
        &self,
        row: usize,
        t: BallType,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(f, "{} |", self.edges.left[row])?;
        for cell in 0..self.size.into() {
            if self.is_ball(Point(row, cell), t) == Some(true) {
                write!(f, "O")?;
            } else {
                write!(f, ".")?;
            }
        }
        writeln!(f, "| {}", self.edges.right[row])
    }

    fn get_edge(&self, edge: Coord) -> Result<Deflection, PuzzleError> {
        if !self.at_edge(edge) {
            return Err(PuzzleError::BadCoordinates);
        }
        self.edges.get(edge).ok_or(PuzzleError::BadCoordinates)
    }

    fn generate_solution_edges(&mut self) -> Result<(), PuzzleError> {
        generate_edges(&self.solution, &mut self.edges)
    }

    pub fn check_guess(&self) -> Result<bool, PuzzleError> {
        // create temporary set of edges
        let mut guess = EdgeRows::new(self.size);
        // generate edge values based on the guess balls
        generate_edges(&self.guess, &mut guess)?;
        // check if the edges match the solution
        Ok(guess == self.edges)
    }
}

fn fmt_horizontal_edge(g: &GridSize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "  +")?;
    for col in 0..g.0 {
        write!(f, "-")?;
    }
    writeln!(f, "+")
}

fn generate_edges(balls: &Balls, edge_rows: &mut EdgeRows) -> Result<(), PuzzleError> {
    // loop through edges
    let edges = edge_rows.edges();
    for edge in edges {
        // check if already a through
        match edge_rows.get(edge).unwrap() {
            Deflection::Through(_) => continue,
            Deflection::EmptyRow | Deflection::EmptyCol => {}
            _ => return Err(PuzzleError::SomethingWentWrong),
        };

        let r = walk_from_edge(balls, edge)?;
        match r {
            Beam::HeadOn => edge_rows
                .add_head_on(edge)
                .ok_or(PuzzleError::SomethingWentWrong)?,
            Beam::Reflect => edge_rows
                .add_reflection(edge)
                .ok_or(PuzzleError::SomethingWentWrong)?,
            Beam::Edge(other_edge, _) => {
                if other_edge == edge {
                    edge_rows
                        .add_reflection(edge)
                        .ok_or(PuzzleError::SomethingWentWrong)?;
                } else {
                    edge_rows
                        .add_through(edge, other_edge)
                        .ok_or(PuzzleError::SomethingWentWrong)?;
                }
            }
            _ => return Err(PuzzleError::SomethingWentWrong),
        };
    }
    Ok(())
}

fn walk_from_edge(balls: &Balls, start: Coord) -> Result<Beam, PuzzleError> {
    let r = edge_step(balls, start)?;
    let (mut next, mut dir) = match r {
        Beam::HeadOn => return Ok(Beam::HeadOn),
        Beam::Reflect => return Ok(Beam::Reflect),
        Beam::Edge(_, _) => return Err(PuzzleError::SomethingWentWrong),
        Beam::Through(n, d) => (n, d),
    };
    loop {
        let r = next_step(
            balls,
            next.try_into().or(Err(PuzzleError::BadCoordinates))?,
            dir,
        )?;
        match r {
            Beam::HeadOn => return Ok(r),
            Beam::Edge(e, _) => {
                if e == start {
                    return Ok(Beam::Reflect);
                } else {
                    return Ok(r);
                }
            }
            Beam::Reflect => return Err(PuzzleError::SomethingWentWrong),
            Beam::Through(n, d) => {
                next = n;
                dir = d;
            }
        };
    }
}

fn edge_step(balls: &Balls, start: Coord) -> Result<Beam, PuzzleError> {
    let dir = match start {
        // top edge
        Coord(-1, _) => Direction::DOWN,
        // left edge
        Coord(_, -1) => Direction::RIGHT,
        // right edge
        Coord(_, y) if y == balls.size.into() => Direction::LEFT,
        // bottom edge
        Coord(x, _) if x == balls.size.into() => Direction::UP,
        // unknown edge
        _ => return Err(PuzzleError::BadCoordinates),
    };
    let (side_left, side_right) = dir.get_sides();
    // check cell in front
    // should always be a cell in front, error if not
    if balls
        .get(
            (start + dir)
                .try_into()
                .or(Err(PuzzleError::BadCoordinates))?,
        )
        .ok_or(PuzzleError::BadCoordinates)?
    {
        // head on
        return Ok(Beam::HeadOn);
    }
    // check cell left
    // there may not be one, so check if cell exists and a ball is there
    let cell_left = start + dir + side_left;
    if balls.size.coord_inside_grid(&cell_left)
        && balls.get(cell_left.try_into().or(Err(PuzzleError::BadCoordinates))?) == Some(true)
    {
        return Ok(Beam::Reflect);
    }
    // check cell right
    // there may not be one, so check if cell exists and a ball is there
    let cell_right = start + dir + side_right;
    if balls.size.coord_inside_grid(&cell_right)
        && balls.get(cell_right.try_into().or(Err(PuzzleError::BadCoordinates))?) == Some(true)
    {
        return Ok(Beam::Reflect);
    }
    Ok(Beam::Through(start + dir, dir))
}

fn next_step(balls: &Balls, start: Point, dir: Coord) -> Result<Beam, PuzzleError> {
    let start: Coord = start.into();
    // check front
    let front = start + dir;

    // if we're at an edge, we've passed through
    if balls.size.at_edge(front) {
        return Ok(Beam::Edge(front, dir));
    }
    // if there is a ball
    if balls
        .get(front.try_into().or(Err(PuzzleError::BadCoordinates))?)
        .ok_or(PuzzleError::BadCoordinates)?
    {
        // return head on
        return Ok(Beam::HeadOn);
    }

    // check left and right for reflection
    let (side_left, side_right) = dir.get_sides();
    let (left, right) = dir.get_side_coords(front);
    let left_edge = balls.size.at_edge(left);
    let right_edge = balls.size.at_edge(right);

    let left_ball = if !left_edge {
        balls
            .get(left.try_into().or(Err(PuzzleError::BadCoordinates))?)
            .ok_or(PuzzleError::BadCoordinates)?
    } else {
        false
    };

    let right_ball = if !right_edge {
        balls
            .get(right.try_into().or(Err(PuzzleError::BadCoordinates))?)
            .ok_or(PuzzleError::BadCoordinates)?
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

fn format_edge_row(row: &EdgeRow, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "   ")?;
    for cell in row {
        write!(f, "{}", cell)?
    }
    writeln!(f, "")
}

impl std::fmt::Display for Puzzle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_guess(f)
    }
}

impl std::fmt::Display for PuzzleSolution<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.puzzle.fmt_solution(f)
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
    fn edge_rows_add_get() {
        let s = GridSize::new(3).unwrap();
        let mut ers = EdgeRows::new(s);
        for index in 0..s.into() {
            let edge = Coord(-1, index);
            assert_eq!(Some(Deflection::EmptyRow), ers.get(edge));
            let edge = Coord(s.into(), index);
            assert_eq!(Some(Deflection::EmptyRow), ers.get(edge));
            let edge = Coord(index, -1);
            assert_eq!(Some(Deflection::EmptyCol), ers.get(edge));
            let edge = Coord(index, s.into());
            assert_eq!(Some(Deflection::EmptyCol), ers.get(edge));
        }

        assert_eq!(Some(()), ers.add_head_on(Coord(-1, 0)));
        assert_eq!(Some(()), ers.add_reflection(Coord(-1, 1)));
        assert_eq!(Some(true), ers.add_through(Coord(-1, 2), Coord(2, -1)));

        assert_eq!(Some(()), ers.add_head_on(Coord(s.into(), 0)));
        assert_eq!(Some(()), ers.add_reflection(Coord(s.into(), 1)));
        assert_eq!(
            Some(true),
            ers.add_through(Coord(s.into(), 2), Coord(2, s.into()))
        );

        assert_eq!(Some(()), ers.add_head_on(Coord(0, -1)));
        assert_eq!(Some(()), ers.add_reflection(Coord(1, -1)));
        assert_eq!(Some(false), ers.add_through(Coord(2, -1), Coord(-1, 2)));

        assert_eq!(Some(()), ers.add_head_on(Coord(0, s.into())));
        assert_eq!(Some(()), ers.add_reflection(Coord(1, s.into())));
        assert_eq!(
            Some(false),
            ers.add_through(Coord(2, s.into()), Coord(s.into(), 2))
        );

        assert_eq!(Some(Deflection::HeadOn), ers.get(Coord(-1, 0)));
        assert_eq!(Some(Deflection::Reflect), ers.get(Coord(-1, 1)));
        assert_eq!(Some(Deflection::Through(BeamId(1))), ers.get(Coord(-1, 2)));

        assert_eq!(Some(Deflection::HeadOn), ers.get(Coord(s.into(), 0),));
        assert_eq!(Some(Deflection::Reflect), ers.get(Coord(s.into(), 1),));
        assert_eq!(
            Some(Deflection::Through(BeamId(2))),
            ers.get(Coord(s.into(), 2),)
        );

        assert_eq!(Some(Deflection::HeadOn), ers.get(Coord(0, -1)));
        assert_eq!(Some(Deflection::Reflect), ers.get(Coord(1, -1)));
        assert_eq!(Some(Deflection::Through(BeamId(1))), ers.get(Coord(2, -1)));

        assert_eq!(Some(Deflection::HeadOn), ers.get(Coord(0, s.into()),));
        assert_eq!(Some(Deflection::Reflect), ers.get(Coord(1, s.into()),));
        assert_eq!(
            Some(Deflection::Through(BeamId(2))),
            ers.get(Coord(2, s.into()),)
        );
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

        let mut balls = Balls::new(GridSize(3));
        balls.add(Point(0, 1)).unwrap();
        balls.add(Point(2, 2)).unwrap();
        // go along top edge, left to right
        assert_eq!(Ok(Beam::Reflect), edge_step(&mut balls, Coord::top(0)));
        assert_eq!(Ok(Beam::HeadOn), edge_step(&mut balls, Coord::top(1)));
        assert_eq!(Ok(Beam::Reflect), edge_step(&mut balls, Coord::top(2)));
        // left edge
        assert_eq!(
            Ok(Beam::Through(Coord(0, 0), Direction::RIGHT)),
            edge_step(&mut balls, Coord::left(0))
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::RIGHT)),
            edge_step(&mut balls, Coord::left(1))
        );
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::RIGHT)),
            edge_step(&mut balls, Coord::left(2))
        );
        // right edge
        assert_eq!(
            Ok(Beam::Through(Coord(0, 2), Direction::LEFT)),
            edge_step(&mut balls, Coord(0, 3))
        );
        assert_eq!(Ok(Beam::Reflect), edge_step(&mut balls, Coord(1, 3)));
        assert_eq!(Ok(Beam::HeadOn), edge_step(&mut balls, Coord(2, 3)));
        // bottom edge
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::UP)),
            edge_step(&mut balls, Coord(3, 0))
        );
        assert_eq!(Ok(Beam::Reflect), edge_step(&mut balls, Coord(3, 1)));
        assert_eq!(Ok(Beam::HeadOn), edge_step(&mut balls, Coord(3, 2)));
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

        let mut balls = Balls::new(GridSize(3));
        balls.add(Point(0, 1)).unwrap();
        balls.add(Point(2, 2)).unwrap();

        // start top, left cell.
        assert_eq!(
            Ok(Beam::HeadOn),
            next_step(&mut balls, Point(0, 0), Direction::RIGHT)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(-1, 0), Direction::UP)),
            next_step(&mut balls, Point(0, 0), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::DOWN)),
            next_step(&mut balls, Point(0, 0), Direction::DOWN)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(0, -1), Direction::LEFT)),
            next_step(&mut balls, Point(0, 0), Direction::LEFT)
        );

        // left, middle cell
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::DOWN)),
            next_step(&mut balls, Point(1, 0), Direction::RIGHT)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::LEFT)),
            next_step(&mut balls, Point(1, 0), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::DOWN)),
            next_step(&mut balls, Point(1, 0), Direction::DOWN)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(1, -1), Direction::LEFT)),
            next_step(&mut balls, Point(1, 0), Direction::LEFT)
        );

        // left bottom cell
        assert_eq!(
            Ok(Beam::Through(Coord(2, 1), Direction::RIGHT)),
            next_step(&mut balls, Point(2, 0), Direction::RIGHT)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 0), Direction::UP)),
            next_step(&mut balls, Point(2, 0), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(3, 0), Direction::DOWN)),
            next_step(&mut balls, Point(2, 0), Direction::DOWN)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(2, -1), Direction::LEFT)),
            next_step(&mut balls, Point(2, 0), Direction::LEFT)
        );

        // middle centre cell
        // left and right directions aren't specified - shouldn't get into that situation
        assert_eq!(
            Ok(Beam::HeadOn),
            next_step(&mut balls, Point(1, 1), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 1), Direction::LEFT)),
            next_step(&mut balls, Point(1, 1), Direction::DOWN)
        );

        // centre bottom cell
        assert_eq!(
            Ok(Beam::HeadOn),
            next_step(&mut balls, Point(2, 1), Direction::RIGHT)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 1), Direction::UP)),
            next_step(&mut balls, Point(2, 1), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(3, 1), Direction::DOWN)),
            next_step(&mut balls, Point(2, 1), Direction::DOWN)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(2, 0), Direction::LEFT)),
            next_step(&mut balls, Point(2, 1), Direction::LEFT)
        );

        // right top cell
        assert_eq!(
            Ok(Beam::Edge(Coord(0, 3), Direction::RIGHT)),
            next_step(&mut balls, Point(0, 2), Direction::RIGHT)
        );
        assert_eq!(
            Ok(Beam::Edge(Coord(-1, 2), Direction::UP)),
            next_step(&mut balls, Point(0, 2), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::Through(Coord(1, 2), Direction::DOWN)),
            next_step(&mut balls, Point(0, 2), Direction::DOWN)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            next_step(&mut balls, Point(0, 2), Direction::LEFT)
        );

        // right middle cell
        // left and right directions aren't specified - shouldn't get into that situation
        assert_eq!(
            Ok(Beam::Through(Coord(1, 2), Direction::RIGHT)),
            next_step(&mut balls, Point(1, 2), Direction::UP)
        );
        assert_eq!(
            Ok(Beam::HeadOn),
            next_step(&mut balls, Point(1, 2), Direction::DOWN)
        );
    }

    #[test]
    fn edges() {
        let s = 4;
        let mut edges = Edges::new(s.try_into().unwrap());
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
    fn coord_pair_eq() {
        let c1 = Coord(-1, 1);
        let c2 = Coord(2, 3);
        let c3 = Coord(4, -5);
        assert_eq!(c1, c1);
        assert_eq!(c2, c2);
        assert_ne!(c1, c2);
        assert_ne!(c2, c1);
        assert_ne!(c3, c1);
        assert_ne!(c2, c3);
        // same coordinates
        assert_eq!(CoordPair(c1, c1), CoordPair(c1, c1));
        // same coordinates, different
        assert_eq!(CoordPair(c1, c2), CoordPair(c1, c2));
        // same coordinates, different order
        assert_eq!(CoordPair(c1, c3), CoordPair(c3, c1));
        // same coordinates, different order
        assert_eq!(CoordPair(c3, c2), CoordPair(c2, c3));

        // different coordinates
        assert_ne!(CoordPair(c1, c1), CoordPair(c2, c2));
        // one common coordinate, same position
        assert_ne!(CoordPair(c1, c2), CoordPair(c3, c2));
        // one common coordinate, different position
        assert_ne!(CoordPair(c1, c2), CoordPair(c2, c3));
    }

    #[test]
    fn edges_from_grid() {
        let s = 3;
        let g = Puzzle::new_empty(s as usize).unwrap();
        let mut edges = g.edge_iter();
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
    fn edges_from_edge_rows() {
        let s = 3;
        let er = EdgeRows::new(s.try_into().unwrap());
        let mut edges = er.edges();
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
        drop(edges);

        let mut edges_top = er.edges_top();
        assert_eq!(Some(Coord(-1, 0)), edges_top.next());
        assert_eq!(Some(Coord(-1, 1)), edges_top.next());
        assert_eq!(Some(Coord(-1, 2)), edges_top.next());
        assert_eq!(None, edges_top.next());
        assert_eq!(None, edges_top.next());
        drop(edges_top);

        let mut edges_left = er.edges_left();
        assert_eq!(Some(Coord(0, -1)), edges_left.next());
        assert_eq!(Some(Coord(1, -1)), edges_left.next());
        assert_eq!(Some(Coord(2, -1)), edges_left.next());
        assert_eq!(None, edges_left.next());
        assert_eq!(None, edges_left.next());
        drop(edges_left);

        let mut edges_right = er.edges_right();
        assert_eq!(Some(Coord(0, s)), edges_right.next());
        assert_eq!(Some(Coord(1, s)), edges_right.next());
        assert_eq!(Some(Coord(2, s)), edges_right.next());
        assert_eq!(None, edges_right.next());
        assert_eq!(None, edges_right.next());
        drop(edges_right);

        let mut edges_bottom = er.edges_bottom();
        assert_eq!(Some(Coord(s, 0)), edges_bottom.next());
        assert_eq!(Some(Coord(s, 1)), edges_bottom.next());
        assert_eq!(Some(Coord(s, 2)), edges_bottom.next());
        assert_eq!(None, edges_bottom.next());
        assert_eq!(None, edges_bottom.next());
        drop(edges_bottom);
    }

    #[test]
    fn edge_rows_eq() {
        let s = 4;
        let mut er1 = EdgeRows::new(s.try_into().unwrap());
        let er_3 = EdgeRows::new(GridSize(3));
        assert_ne!(er1, er_3);
        drop(er_3);
        let mut er2 = EdgeRows::new(s.try_into().unwrap());
        assert_eq!(er1, er2);
        er1.get(Coord(-1, 2));
        assert_eq!(er1, er2);
        er2.get(Coord(2, s));
        assert_eq!(er1, er2);
        assert_eq!(er2, er1);

        er1.add_head_on(Coord(-1, 2));
        assert_ne!(er1, er2);

        er2.add_head_on(Coord(-1, 2));
        assert_eq!(er1, er2);

        er2.add_reflection(Coord(-1, 3));
        assert_ne!(er2, er1);
        er1.add_reflection(Coord(-1, 3));
        assert_eq!(er2, er1);

        er1.add_reflection(Coord(-1, 1));
        er1.add_reflection(Coord(0, -1));
        assert_ne!(er1, er2);
        er2.add_reflection(Coord(-1, 1));
        er2.add_reflection(Coord(0, -1));
        assert_eq!(er1, er2);
        er1.add_reflection(Coord(0, -1));
        assert_eq!(er1, er2);

        er1.add_head_on(Coord(s, 0));
        assert_ne!(er1, er2);
        er2.add_head_on(Coord(s, 0));
        assert_eq!(er1, er2);

        er1.add_reflection(Coord(1, s));
        assert_ne!(er1, er2);
        er2.add_reflection(Coord(1, s));
        assert_eq!(er1, er2);

        er1.add_through(Coord(2, -1), Coord(s, 2));
        assert_ne!(er1, er2);
        er1.add_through(Coord(2, s), Coord(s, 1));
        assert_ne!(er1, er2);
        er2.add_through(Coord(2, s), Coord(s, 1));
        assert_ne!(er1, er2);
        er2.add_through(Coord(2, -1), Coord(s, 2));
        assert_eq!(er1, er2);

        er1.add_head_on(Coord(1, -1));
        er2.add_reflection(Coord(1, -1));
        assert_ne!(er1, er2);

        let mut er1 = EdgeRows::new(s.try_into().unwrap());
        let mut er2 = EdgeRows::new(s.try_into().unwrap());
        er1.add_through(Coord(2, -1), Coord(s, 2));
        assert_ne!(er1, er2);
        er2.add_through(Coord(2, -1), Coord(s, 1));
        assert_ne!(er1, er2);
        er1.add_through(Coord(2, s), Coord(s, 1));
        assert_ne!(er2, er1);
        er2.add_through(Coord(2, s), Coord(s, 2));
        assert_ne!(er1, er2);
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

        let mut balls = Balls::new(GridSize(3));
        balls.add(Point(0, 1)).unwrap();
        balls.add(Point(2, 2)).unwrap();

        // Top edge
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(-1, 0)));
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(-1, 1)));
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(-1, 2)));

        // Left edge
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(0, -1)));
        assert_eq!(
            Ok(Beam::Edge(Coord(3, 0), Direction::DOWN)),
            walk_from_edge(&mut balls, Coord(1, -1))
        );
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(2, -1)));

        // Right edge
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(0, 3)));
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(1, 3)));
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(2, 3)));

        // Bottom edge
        assert_eq!(
            Ok(Beam::Edge(Coord(1, -1), Direction::LEFT)),
            walk_from_edge(&mut balls, Coord(3, 0))
        );
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(3, 1)));
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(3, 2)));
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

        let mut balls = Balls::new(GridSize(3));
        balls.add(Point(0, 0)).unwrap();
        balls.add(Point(0, 2)).unwrap();

        // Top edge
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(-1, 0)));
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(-1, 1)));
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(-1, 2)));

        // Left edge
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(0, -1)));
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(1, -1)));
        assert_eq!(
            Ok(Beam::Edge(Coord(2, 3), Direction::RIGHT)),
            walk_from_edge(&mut balls, Coord(2, -1))
        );

        // Right edge
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(0, 3)));
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(1, 3)));
        assert_eq!(
            Ok(Beam::Edge(Coord(2, -1), Direction::LEFT)),
            walk_from_edge(&mut balls, Coord(2, 3))
        );

        // Bottom edge
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(3, 0)));
        assert_eq!(Ok(Beam::Reflect), walk_from_edge(&mut balls, Coord(3, 1)));
        assert_eq!(Ok(Beam::HeadOn), walk_from_edge(&mut balls, Coord(3, 2)));
    }

    // Some interesting cases I've found playing a different implementation of this game

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

        let mut g = Puzzle::new_empty(3).unwrap();
        g.add_ball_solution(Point(0, 1)).unwrap();
        g.add_ball_solution(Point(2, 2)).unwrap();
        g.generate_solution_edges().unwrap();

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

        let mut g = Puzzle::new_empty(3).unwrap();
        g.add_ball_solution(Point(0, 0)).unwrap();
        g.add_ball_solution(Point(0, 2)).unwrap();

        g.generate_solution_edges().unwrap();

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
        //    H |.....O.O..| H
        //    3 |..........| 7
        //    4 |..........| 4
        //    5 |..........| 5
        //      +----------+
        //       H2H63HRH78

        let mut g = Puzzle::new_empty(10).unwrap();
        g.add_ball_solution(Point(1, 5)).unwrap();
        g.add_ball_solution(Point(3, 0)).unwrap();
        g.add_ball_solution(Point(3, 6)).unwrap();
        g.add_ball_solution(Point(4, 2)).unwrap();
        g.add_ball_solution(Point(6, 5)).unwrap();
        g.add_ball_solution(Point(6, 7)).unwrap();
        g.generate_solution_edges().unwrap();

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

        let mut expected = EdgeRows::new(GridSize(10));

        // unused lines left to make copying easier

        // top side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 0)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 2)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 3)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 4)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 5)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 8)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 9)));

        // left side head/reflect
        // assert_eq!(Some(()), expected.add_head_on(Coord(0, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(1, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(2, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(3, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(4, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(5, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(6, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(7, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(8, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, -1)));

        // right side head/reflect
        // assert_eq!(Some(()), expected.add_head_on(Coord(0, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(1, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(2, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(3, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(4, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(5, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(6, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(7, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(8, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, 10)));

        // bottom side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 0)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 2)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 3)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 4)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 5)));
        assert_eq!(Some(()), expected.add_reflection(Coord(10, 6)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 8)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 9)));

        // throughs from left, then top, then right, then bottom
        assert_eq!(Some(true), expected.add_through(Coord(0, -1), Coord(-1, 4)));
        assert_eq!(Some(true), expected.add_through(Coord(5, -1), Coord(10, 1)));
        assert_eq!(Some(true), expected.add_through(Coord(7, -1), Coord(10, 4)));
        assert_eq!(Some(true), expected.add_through(Coord(8, -1), Coord(8, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(9, -1), Coord(9, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 1), Coord(10, 3)));
        assert_eq!(
            Some(false),
            expected.add_through(Coord(-1, 4), Coord(0, -1))
        );
        assert_eq!(Some(true), expected.add_through(Coord(-1, 6), Coord(0, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 7), Coord(2, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 8), Coord(5, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 9), Coord(10, 9)));
        assert_eq!(
            Some(false),
            expected.add_through(Coord(-1, 1), Coord(10, 3))
        );
        assert_eq!(Some(true), expected.add_through(Coord(7, 10), Coord(10, 8)));

        let expected = expected;

        // ensure no empty edges
        assert_eq!(false, expected.are_empty_edges());
        assert_eq!(false, g.edges.are_empty_edges());

        // ensure edge rows are equal
        assert_eq!(expected, g.edges);
    }

    #[test]
    fn check_solution_edges_10x_6_2() {
        // define a 10x10 grid, with 10 balls, and expected edges
        //       HHRRHRH123
        //      +----------+
        //    H |....O.....| H
        //    H |..........| 1
        //    H |......O...| H
        //    R |..........| 4
        //    H |OO.O.O....| H
        //    H |O.........| 5
        //    R |.O........| H
        //    H |..........| H
        //    R |..O.......| H
        //    H |O.........| 6
        //      +----------+
        //       HRH6RH5423

        let mut g = Puzzle::new_empty(10).unwrap();
        g.add_ball_solution(Point(0, 4)).unwrap();
        g.add_ball_solution(Point(2, 6)).unwrap();
        g.add_ball_solution(Point(4, 0)).unwrap();
        g.add_ball_solution(Point(4, 1)).unwrap();
        g.add_ball_solution(Point(4, 3)).unwrap();
        g.add_ball_solution(Point(4, 5)).unwrap();
        g.add_ball_solution(Point(5, 0)).unwrap();
        g.add_ball_solution(Point(6, 1)).unwrap();
        g.add_ball_solution(Point(8, 2)).unwrap();
        g.add_ball_solution(Point(9, 0)).unwrap();
        g.generate_solution_edges().unwrap();

        let mut expected = EdgeRows::new(GridSize(10));

        // unused lines left to make copying easier

        // top side head
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 0)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 2)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 3)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 4)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 5)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 8)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 9)));

        // top side reflect
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 0)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(-1, 2)));
        assert_eq!(Some(()), expected.add_reflection(Coord(-1, 3)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 4)));
        assert_eq!(Some(()), expected.add_reflection(Coord(-1, 5)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 7)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 8)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 9)));

        // left side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(0, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(1, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(2, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(3, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(4, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(5, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(6, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(7, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(8, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(9, -1)));

        // right side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(0, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(1, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(2, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(3, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(4, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(5, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(6, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(7, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(8, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, 10)));

        // bottom side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 0)));
        assert_eq!(Some(()), expected.add_reflection(Coord(10, 1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 2)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 3)));
        assert_eq!(Some(()), expected.add_reflection(Coord(10, 4)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 5)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(10, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 8)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 9)));

        // throughs from left, then top, then right, then bottom
        assert_eq!(Some(true), expected.add_through(Coord(-1, 7), Coord(1, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 8), Coord(10, 8)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 9), Coord(10, 9)));
        assert_eq!(Some(true), expected.add_through(Coord(3, 10), Coord(10, 7)));
        assert_eq!(Some(true), expected.add_through(Coord(5, 10), Coord(10, 6)));
        assert_eq!(Some(true), expected.add_through(Coord(9, 10), Coord(10, 3)));

        let expected = expected;

        // ensure no empty edges
        assert_eq!(false, expected.are_empty_edges());
        assert_eq!(false, g.edges.are_empty_edges());

        // ensure edge rows are equal
        assert_eq!(expected, g.edges);
    }

    #[test]
    fn check_solution_edges_10x_6_3() {
        // define a 10x10 grid, with 7 balls, and expected edges
        //       HRHRHH65HH
        //      +----------+
        //    R |..O.......| H
        //    H |O.........| 6
        //    R |.....O....| R
        //    1 |.........O| H
        //    H |..........| R
        //    H |...O......| H
        //    H |...O......| H
        //    H |..........| 4
        //    H |..O.......| H
        //    2 |..........| 3
        //      +----------+
        //       H2H34HH51H

        let mut g = Puzzle::new_empty(10).unwrap();
        g.add_ball_solution(Point(0, 2)).unwrap();
        g.add_ball_solution(Point(1, 0)).unwrap();
        g.add_ball_solution(Point(2, 5)).unwrap();
        g.add_ball_solution(Point(3, 9)).unwrap();
        g.add_ball_solution(Point(5, 3)).unwrap();
        g.add_ball_solution(Point(6, 3)).unwrap();
        g.add_ball_solution(Point(8, 2)).unwrap();
        g.generate_solution_edges().unwrap();

        let mut expected = EdgeRows::new(GridSize(10));

        // unused lines left to make copying easier

        // top side head
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 0)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 2)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 3)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 4)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 5)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 7)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 8)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 9)));

        // top side reflect
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 0)));
        assert_eq!(Some(()), expected.add_reflection(Coord(-1, 1)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 2)));
        assert_eq!(Some(()), expected.add_reflection(Coord(-1, 3)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 4)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 5)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 7)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 8)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 9)));

        // left side head/reflect
        assert_eq!(Some(()), expected.add_reflection(Coord(0, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(1, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(2, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(3, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(4, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(5, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(6, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(7, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(8, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, -1)));

        // right side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(0, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(1, 10)));
        assert_eq!(Some(()), expected.add_reflection(Coord(2, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(3, 10)));
        assert_eq!(Some(()), expected.add_reflection(Coord(4, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(5, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(6, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(7, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(8, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, 10)));

        // bottom side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 0)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 2)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 3)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 4)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 5)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 8)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 9)));

        // throughs from left, then top, then right, then bottom
        assert_eq!(Some(true), expected.add_through(Coord(3, -1), Coord(10, 8)));
        assert_eq!(Some(true), expected.add_through(Coord(9, -1), Coord(10, 1)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 3), Coord(9, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 4), Coord(7, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 7), Coord(-1, 7)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 6), Coord(1, 10)));

        let expected = expected;

        // ensure no empty edges
        assert_eq!(false, expected.are_empty_edges());
        assert_eq!(false, g.edges.are_empty_edges());

        // ensure edge rows are equal
        assert_eq!(expected, g.edges);
    }

    #[test]
    fn check_solution_edges_10x_6_4() {
        // define a 10x10 grid, with 5 balls, and expected edges
        //       H43Hbc87a9
        //      +----------+
        //    1 |..........| 1
        //    2 |..........| 2
        //    3 |..........| b
        //    H |...O......| H
        //    R |..........| R
        //    4 |...O......| H
        //    R |..O.......| H
        //    H |O.........| c
        //    R |....O.....| H
        //    5 |..........| 6
        //      +----------+
        //       HHH5H687a9

        let mut g = Puzzle::new_empty(10).unwrap();
        g.add_ball_solution(Point(3, 3)).unwrap();
        g.add_ball_solution(Point(5, 3)).unwrap();
        g.add_ball_solution(Point(6, 2)).unwrap();
        g.add_ball_solution(Point(7, 0)).unwrap();
        g.add_ball_solution(Point(8, 4)).unwrap();
        g.generate_solution_edges().unwrap();

        let mut expected = EdgeRows::new(GridSize(10));

        // unused lines left to make copying easier

        // top side head
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 0)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 2)));
        assert_eq!(Some(()), expected.add_head_on(Coord(-1, 3)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 4)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 5)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 8)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(-1, 9)));

        // top side reflect
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 0)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 1)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 2)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 3)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 4)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 5)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 6)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 7)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 8)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(-1, 9)));

        // left side head/reflect
        // assert_eq!(Some(()), expected.add_reflection(Coord(0, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(1, -1)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(2, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(3, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(4, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(5, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(6, -1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(7, -1)));
        assert_eq!(Some(()), expected.add_reflection(Coord(8, -1)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, -1)));

        // right side head/reflect
        // assert_eq!(Some(()), expected.add_head_on(Coord(0, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(1, 10)));
        // assert_eq!(Some(()), expected.add_reflection(Coord(2, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(3, 10)));
        assert_eq!(Some(()), expected.add_reflection(Coord(4, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(5, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(6, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(7, 10)));
        assert_eq!(Some(()), expected.add_head_on(Coord(8, 10)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(9, 10)));

        // bottom side head/reflect
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 0)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 1)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 2)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 3)));
        assert_eq!(Some(()), expected.add_head_on(Coord(10, 4)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 5)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 6)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 7)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 8)));
        // assert_eq!(Some(()), expected.add_head_on(Coord(10, 9)));

        // throughs from left, then top, then right, then bottom
        assert_eq!(Some(true), expected.add_through(Coord(0, -1), Coord(0, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(1, -1), Coord(1, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(2, -1), Coord(-1, 2)));
        assert_eq!(Some(true), expected.add_through(Coord(5, -1), Coord(-1, 1)));
        assert_eq!(Some(true), expected.add_through(Coord(9, -1), Coord(10, 3)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 5), Coord(9, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 6), Coord(-1, 6)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 7), Coord(-1, 7)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 8), Coord(-1, 8)));
        assert_eq!(Some(true), expected.add_through(Coord(10, 9), Coord(-1, 9)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 4), Coord(2, 10)));
        assert_eq!(Some(true), expected.add_through(Coord(-1, 5), Coord(7, 10)));

        let expected = expected;

        // ensure no empty edges
        assert_eq!(false, expected.are_empty_edges());
        assert_eq!(false, g.edges.are_empty_edges());

        // ensure edge rows are equal
        assert_eq!(expected, g.edges);
    }

    #[test]
    fn check_solution_edges_10x_6_5() {
        // define a 10x10 grid, with 8 balls, and expected edges
        let input = r#"
         4H3H91HHRH
        +----------+
      H |.........O| H
      1 |..........| R
      H |......O...| H
      2 |........O.| H
      3 |..........| 8
      H |...O......| H
      4 |..........| 2
      H |.O..O.....| 9
      5 |......O...| 8
      6 |........O.| H
        +----------+
         5H77H6HRHR
   "#;

        let mut got = Puzzle::new_empty(10).unwrap();
        got.add_ball_solution(Point(0, 9)).unwrap();
        got.add_ball_solution(Point(2, 6)).unwrap();
        got.add_ball_solution(Point(3, 8)).unwrap();
        got.add_ball_solution(Point(5, 3)).unwrap();
        got.add_ball_solution(Point(7, 1)).unwrap();
        got.add_ball_solution(Point(7, 4)).unwrap();
        got.add_ball_solution(Point(8, 6)).unwrap();
        got.add_ball_solution(Point(9, 8)).unwrap();
        got.generate_solution_edges().unwrap();

        let mut expected = parse_grid(input, 10).unwrap();

        assert_eq!(expected.edges, got.edges);
        assert_eq!(expected.solution, got.solution);

        // ensure no empty edges
        assert_eq!(false, expected.edges.are_empty_edges());
        assert_eq!(false, got.edges.are_empty_edges());
    }

    #[test]
    fn check_solution_edges_10x_6_6() {
        // define a 10x10 grid, with 8 balls, and expected edges
        let input = r#"
         21HHRHRH8H
        +----------+
      1 |.....O....| H
      2 |..O.......| 8
      H |.O.....O..| H
      3 |.......O..| R
      H |.......O.O| H
      H |..........| R
      H |......O...| H
      4 |..........| 7
      5 |..........| 5
      6 |..........| 6
        +----------+
         3HHHH4H7RH
   "#;

        let expected = parse_grid(input, 10).unwrap();

        let mut got = Puzzle::new_empty(10).unwrap();

        for p in expected.solution.iter() {
            got.add_ball_solution(p).unwrap();
        }

        got.generate_solution_edges().unwrap();

        assert_eq!(expected.edges, got.edges);
        assert_eq!(expected.solution, got.solution);

        // ensure no empty edges
        assert_eq!(false, expected.edges.are_empty_edges());
        assert_eq!(false, got.edges.are_empty_edges());
    }

    #[test]
    fn check_guess_attempt_solution_edges_10x_6_6() {
        // define a 10x10 grid, with 8 balls, and expected edges
        let input = r#"
         21HHRHRH8H
        +----------+
      1 |.....O....| H
      2 |..O.......| 8
      H |.O.....O..| H
      3 |.......O..| R
      H |.......O.O| H
      H |..........| R
      H |......O...| H
      4 |..........| 7
      5 |..........| 5
      6 |..........| 6
        +----------+
         3HHHH4H7RH
   "#;

        let expected = parse_grid(input, 10).unwrap();

        let mut got = Puzzle::new_empty(10).unwrap();

        for p in expected.solution.iter() {
            got.add_ball_solution(p).unwrap();
        }

        got.generate_solution_edges().unwrap();

        assert_eq!(expected.edges, got.edges);
        assert_eq!(expected.solution, got.solution);

        // ensure no empty edges
        assert_eq!(false, expected.edges.are_empty_edges());
        assert_eq!(false, got.edges.are_empty_edges());

        // add guess balls
        for b in got.solution.iter() {
            got.add_ball_guess(b).unwrap();
        }
        assert_eq!(Ok(true), got.check_guess());
    }

    #[derive(Debug, PartialEq, Eq)]
    struct ThroughHalf {
        id: BeamId,
        edge: Coord,
    }

    use std::cmp::Ordering;

    impl std::cmp::PartialOrd for ThroughHalf {
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
            Some(self.cmp(other))
        }
    }

    impl std::cmp::Ord for ThroughHalf {
        fn cmp(&self, other: &Self) -> Ordering {
            match self.id.cmp(&other.id) {
                Ordering::Less => Ordering::Less,
                Ordering::Greater => Ordering::Greater,
                Ordering::Equal => self.edge.cmp(&other.edge),
            }
        }
    }

    fn parse_edge_character(c: char) -> Option<Deflection> {
        Some(match c {
            'H' => Deflection::HeadOn,
            'R' => Deflection::Reflect,
            x if x.is_ascii_digit() => Deflection::Through(BeamId(x as u8 - b'0')),
            x if x.is_ascii_lowercase() => Deflection::Through(BeamId(x as u8 - b'a' + 10)),
            _ => return None,
        })
    }

    // Parse a horizontal edge row (top or bottom) from a line of ASCII, placing the result into the grid
    // Through edges are ignored
    fn parse_horizontal_edge_row(
        er: &mut EdgeRows,
        s: Side,
        line: &str,
    ) -> Option<Vec<ThroughHalf>> {
        let row = match s {
            Side::Top => -1,
            Side::Bottom => er.size.into(),
            _ => return None,
        };
        let line = line.trim();
        if line.len() != er.size.into() {
            return None;
        }
        let mut throughs = Vec::new();
        for (i, c) in line.chars().enumerate() {
            let edge = Coord(row, i as isize);
            match parse_edge_character(c) {
                Some(Deflection::HeadOn) => er.add_head_on(edge)?,
                Some(Deflection::Reflect) => er.add_reflection(edge)?,
                Some(Deflection::Through(id)) => throughs.push(ThroughHalf { edge, id }),
                _ => return None,
            }
        }
        Some(throughs)
    }

    #[test]
    fn test_parse_horizontal_edge_row() {
        let top = "  H6HR1H9ab8 ";
        let bottom = "  HRH6RH5423 ";
        let mut er = EdgeRows::new(GridSize::new(10).unwrap());
        let topThroughs = vec![
            ThroughHalf {
                id: BeamId(6),
                edge: Coord(-1, 1),
            },
            ThroughHalf {
                id: BeamId(1),
                edge: Coord(-1, 4),
            },
            ThroughHalf {
                id: BeamId(9),
                edge: Coord(-1, 6),
            },
            ThroughHalf {
                id: BeamId(10),
                edge: Coord(-1, 7),
            },
            ThroughHalf {
                id: BeamId(11),
                edge: Coord(-1, 8),
            },
            ThroughHalf {
                id: BeamId(8),
                edge: Coord(-1, 9),
            },
        ];
        let bottomThroughs = vec![
            ThroughHalf {
                id: BeamId(6),
                edge: Coord(10, 3),
            },
            ThroughHalf {
                id: BeamId(5),
                edge: Coord(10, 6),
            },
            ThroughHalf {
                id: BeamId(4),
                edge: Coord(10, 7),
            },
            ThroughHalf {
                id: BeamId(2),
                edge: Coord(10, 8),
            },
            ThroughHalf {
                id: BeamId(3),
                edge: Coord(10, 9),
            },
        ];
        assert_eq!(
            Some(topThroughs),
            parse_horizontal_edge_row(&mut er, Side::Top, top)
        );
        assert_eq!(
            Some(bottomThroughs),
            parse_horizontal_edge_row(&mut er, Side::Bottom, bottom)
        );

        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(-1, 0)));
        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(-1, 2)));
        assert_eq!(Some(Deflection::Reflect), er.get(Coord(-1, 3)));
        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(-1, 5)));
        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(10, 0)));
        assert_eq!(Some(Deflection::Reflect), er.get(Coord(10, 1)));
        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(10, 2)));
        assert_eq!(Some(Deflection::Reflect), er.get(Coord(10, 4)));
        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(10, 5)));
    }

    // Parse a row of a grid text representation into the grid, added left and right edges and any balls.
    fn parse_grid_row(
        edges: &mut EdgeRows,
        balls: &mut Balls,
        row: usize,
        line: &str,
    ) -> Option<Vec<ThroughHalf>> {
        let line = line.trim();
        let parts: Vec<&str> = line.split_whitespace().collect();
        if parts.len() != 3 {
            return None;
        }
        let mut throughs = Vec::new();
        let left = parts[0].trim().chars().nth(0)?;
        let edge = Coord(row as isize, -1);
        match parse_edge_character(left) {
            Some(Deflection::HeadOn) => edges.add_head_on(edge)?,
            Some(Deflection::Reflect) => edges.add_reflection(edge)?,
            Some(Deflection::Through(id)) => throughs.push(ThroughHalf { edge, id }),
            _ => return None,
        }
        let right = parts[2].trim().chars().nth(0)?;
        let edge = Coord(row as isize, edges.size.into());
        match parse_edge_character(right) {
            Some(Deflection::HeadOn) => edges.add_head_on(edge)?,
            Some(Deflection::Reflect) => edges.add_reflection(edge)?,
            Some(Deflection::Through(id)) => throughs.push(ThroughHalf { edge, id }),
            _ => return None,
        }

        let middle = parts[1].trim().trim_matches('|');
        if middle.len() != edges.size.into() {
            return None;
        }
        for (c, d) in middle.chars().enumerate() {
            match d {
                '.' => continue,
                'O' => balls.add(Point(row, c)).ok()?,
                _ => return None,
            }
        }
        Some(throughs)
    }

    #[test]
    fn test_parse_grid_row() {
        let r0 = " H |OO.O.O....| 5";
        let r1 = "   R |.O........| H";
        let gs = GridSize::new(10).unwrap();
        let mut er = EdgeRows::new(gs);
        let mut balls = Balls::new(gs);
        let through0 = vec![ThroughHalf {
            id: BeamId(5),
            edge: Coord(0, 10),
        }];
        let through1 = vec![];
        assert_eq!(Some(through0), parse_grid_row(&mut er, &mut balls, 0, r0));
        assert_eq!(Some(through1), parse_grid_row(&mut er, &mut balls, 1, r1));

        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(0, -1)));
        assert_eq!(Some(Deflection::Reflect), er.get(Coord(1, -1)));
        assert_eq!(Some(Deflection::HeadOn), er.get(Coord(1, gs.into())));

        assert_eq!(Some(true), balls.get(Point(0, 0)));
        assert_eq!(Some(true), balls.get(Point(0, 1)));
        assert_eq!(Some(false), balls.get(Point(0, 2)));
        assert_eq!(Some(true), balls.get(Point(0, 3)));
        assert_eq!(Some(false), balls.get(Point(0, 4)));
        assert_eq!(Some(true), balls.get(Point(0, 5)));
        assert_eq!(Some(false), balls.get(Point(0, 6)));
        assert_eq!(Some(false), balls.get(Point(0, 7)));
        assert_eq!(Some(false), balls.get(Point(0, 8)));
        assert_eq!(Some(false), balls.get(Point(0, 9)));
        assert_eq!(Some(false), balls.get(Point(1, 0)));
        assert_eq!(Some(true), balls.get(Point(1, 1)));
        assert_eq!(Some(false), balls.get(Point(1, 2)));
        assert_eq!(Some(false), balls.get(Point(1, 3)));
        assert_eq!(Some(false), balls.get(Point(1, 4)));
        assert_eq!(Some(false), balls.get(Point(1, 5)));
        assert_eq!(Some(false), balls.get(Point(1, 6)));
        assert_eq!(Some(false), balls.get(Point(1, 7)));
        assert_eq!(Some(false), balls.get(Point(1, 8)));
        assert_eq!(Some(false), balls.get(Point(1, 9)));
    }

    // Parse a grid represented as ASCII into a Grid object
    // Format example:
    //     HRH
    //    +---+
    //  H |O.O| H
    //  R |...| R
    //  1 |...| 1
    //    +---+
    //     HRH
    fn parse_grid(text: &str, size: usize) -> Option<Puzzle> {
        let mut g = Puzzle::new_empty(size).ok()?;
        let text = text.trim();
        let mut throughs = Vec::new();
        for (i, line) in text.lines().enumerate() {
            throughs.append(&mut match i {
                // first line is top row
                0 => parse_horizontal_edge_row(&mut g.edges, Side::Top, line)?,
                // skip border line, then we have grid lines
                i if i >= 2 && i <= size + 1 => {
                    parse_grid_row(&mut g.edges, &mut g.solution, i - 2, line)?
                }
                // skip border line, then we have the bottom edge
                i if i == size + 3 => parse_horizontal_edge_row(&mut g.edges, Side::Bottom, line)?,
                _ => continue,
            });
        }

        // sort ThroughHalfs, which is first by ID. So we should get sequential IDs coming through in pairs
        throughs.sort();

        while throughs.len() > 0 {
            let t1 = throughs.pop().unwrap();
            // if there isn't one, it's an error so return none
            let t2 = throughs.pop()?;
            if t1.id != t2.id {
                return None;
            }
            if Some(true) != g.edges.add_through(t1.edge, t2.edge) {
                return None;
            }
        }
        Some(g)
    }

    #[test]
    fn test_parse_grid() {
        let input = r#"
             HRH
            +---+
          H |O.O| H
          R |...| R
          1 |...| 1
            +---+
             HRH
        "#;
        let gs = GridSize::new(3).unwrap();
        let mut expected = Puzzle::new_empty(gs.into()).unwrap();
        assert_eq!(Ok(()), expected.add_ball_solution(Point(0, 0)));
        assert_eq!(Ok(()), expected.add_ball_solution(Point(0, 2)));
        assert_eq!(Ok(()), expected.generate_solution_edges());

        let got = parse_grid(input, 3).unwrap();
        assert_eq!(expected.size, got.size);
        assert_eq!(expected.edges, got.edges);

        assert_eq!(Some(true), got.is_ball(Point(0, 0), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(0, 1), BallType::Solution));
        assert_eq!(Some(true), got.is_ball(Point(0, 2), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(1, 0), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(1, 1), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(1, 2), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(2, 0), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(2, 1), BallType::Solution));
        assert_eq!(Some(false), got.is_ball(Point(2, 2), BallType::Solution));
        assert_eq!(Ok(Deflection::HeadOn), got.get_edge(Coord(-1, 0)));
        assert_eq!(Ok(Deflection::Reflect), got.get_edge(Coord(-1, 1)));
        assert_eq!(Ok(Deflection::HeadOn), got.get_edge(Coord(-1, 2)));
        assert_eq!(Ok(Deflection::HeadOn), got.get_edge(Coord(3, 0)));
        assert_eq!(Ok(Deflection::Reflect), got.get_edge(Coord(3, 1)));
        assert_eq!(Ok(Deflection::HeadOn), got.get_edge(Coord(3, 2)));
        assert_eq!(Ok(Deflection::HeadOn), got.get_edge(Coord(0, -1)));
        assert_eq!(Ok(Deflection::Reflect), got.get_edge(Coord(1, -1)));
        assert_eq!(Ok(Deflection::HeadOn), got.get_edge(Coord(0, 3)));
        assert_eq!(Ok(Deflection::Reflect), got.get_edge(Coord(1, 3)));
        assert_eq!(
            Ok(Deflection::Through(BeamId(1))),
            got.get_edge(Coord(2, -1))
        );
        assert_eq!(
            Ok(Deflection::Through(BeamId(1))),
            got.get_edge(Coord(2, 3))
        );
    }

    #[test]
    fn generate_new_puzzle_with_balls() {
        let p = Puzzle::new(Puzzle::MIN_SIZE).unwrap();
        assert_eq!(true, p.solution.count > 0);
    }
}
