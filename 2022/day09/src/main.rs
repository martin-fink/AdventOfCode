use std::collections::HashSet;
use std::ops;
use std::str::FromStr;

#[derive(Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl FromStr for Direction {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "U" => Ok(Direction::Up),
            "D" => Ok(Direction::Down),
            "L" => Ok(Direction::Left),
            "R" => Ok(Direction::Right),
            _ => Err("Unexpected char".into()),
        }
    }
}

impl ops::AddAssign<Direction> for Position {
    fn add_assign(&mut self, rhs: Direction) {
        match rhs {
            Direction::Up => self.1 += 1,
            Direction::Down => self.1 -= 1,
            Direction::Left => self.0 -= 1,
            Direction::Right => self.0 += 1,
        }
    }
}

type Position = (isize, isize);
struct Rope<const N: usize> {
    knots: [Position; N],
}

impl<const N: usize> Rope<N> {
    fn new() -> Rope<N> {
        Rope { knots: [(0, 0); N] }
    }

    fn move_rope(self: &mut Rope<N>, dir: Direction) {
        self.knots[0] += dir;

        for i in 1..N {
            let x_diff = self.knots[i - 1].0 - self.knots[i].0;
            let y_diff = self.knots[i - 1].1 - self.knots[i].1;

            if x_diff.abs() > 1 || y_diff.abs() > 1 {
                self.knots[i].0 += x_diff.signum();
                self.knots[i].1 += y_diff.signum();
            }
        }
    }

    fn get_tail(self: &Rope<N>) -> Position {
        self.knots[N - 1]
    }
}

fn main() {
    let input = common::read_lines("data/input.txt").expect("File could not be read");

    let instructions = input
        .iter()
        .map(|line| line.split(' ').collect::<Vec<_>>())
        .map(|split| {
            (
                split[0]
                    .parse::<Direction>()
                    .expect("Could not parse direction"),
                split[1].parse::<u32>().expect("Could not parse amount"),
            )
        })
        .flat_map(|(dir, n)| vec![dir].repeat(n as usize))
        .collect::<Vec<_>>();

    let mut rope: Rope<2> = Rope::new();

    let mut visited_positions: HashSet<Position> = HashSet::new();
    visited_positions.insert(rope.get_tail());

    for instruction in instructions.iter() {
        rope.move_rope(*instruction);
        visited_positions.insert(rope.get_tail());
        println!()
    }

    print_grid(&visited_positions);

    println!("{:?}", visited_positions.len());
    let mut rope: Rope<10> = Rope::new();

    let mut visited_positions: HashSet<Position> = HashSet::new();
    visited_positions.insert(rope.get_tail());

    for instruction in instructions {
        rope.move_rope(instruction);
        visited_positions.insert(rope.get_tail());
        println!()
    }

    print_grid(&visited_positions);

    println!("{:?}", visited_positions.len());
}

fn print_grid(visited_positions: &HashSet<Position>) {
    let max_x = visited_positions.iter().map(|(x, _)| *x).max().unwrap();
    let max_y = visited_positions.iter().map(|(_, y)| *y).max().unwrap();

    for j in (0..max_y).rev() {
        for i in 0..max_x {
            if i == 0 && j == 0 {
                print!("S");
            } else if visited_positions.contains(&(i, j)) {
                print!("#")
            } else {
                print!(".")
            }
        }
        println!();
    }
}
