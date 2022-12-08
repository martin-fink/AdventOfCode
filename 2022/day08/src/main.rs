use std::ops;

struct Tree {
    is_visible: bool,
    height: u8,
}

#[derive(Clone, Copy)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl ops::Add<Direction> for (isize, isize) {
    type Output = (isize, isize);

    fn add(self, rhs: Direction) -> Self::Output {
        match rhs {
            Direction::Up => (self.0, self.1 - 1),
            Direction::Down => (self.0, self.1 + 1),
            Direction::Left => (self.0 - 1, self.1),
            Direction::Right => (self.0 + 1, self.1),
        }
    }
}

fn get_visible_trees(
    forest: &Vec<Vec<Tree>>,
    pos: (isize, isize),
    height: u8,
    dir: Direction,
) -> usize {
    if pos.0 < 0 || pos.1 < 0 || pos.0 >= forest.len() as isize || pos.1 >= forest[0].len() as isize
    {
        return 0;
    }
    let (i, j) = (pos.0 as usize, pos.1 as usize);

    if forest[i][j].height >= height {
        1
    } else {
        1 + get_visible_trees(forest, pos + dir, height, dir)
    }
}

fn main() {
    let input = common::read_file("data/input.txt").expect("File could not be read");

    let mut forest = input
        .split('\n')
        .map(|line| {
            line.chars()
                .map(|c| Tree {
                    height: c.to_digit(10).expect("Not a digit") as u8,
                    is_visible: false,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    for i in 0..forest.len() {
        let mut tallest = forest[i][0].height;

        for j in 0..forest[0].len() {
            forest[i][j].is_visible |= j == 0 || forest[i][j].height > tallest;
            tallest = tallest.max(forest[i][j].height);
        }

        tallest = forest[i][forest[i].len() - 1].height;
        for j in (0..forest[0].len()).rev() {
            forest[i][j].is_visible |= j == forest[i].len() - 1 || forest[i][j].height > tallest;
            tallest = tallest.max(forest[i][j].height);
        }
    }

    for j in 0..forest[0].len() {
        let mut tallest = forest[0][j].height;

        for i in 0..forest.len() {
            forest[i][j].is_visible |= i == 0 || forest[i][j].height > tallest;
            tallest = tallest.max(forest[i][j].height);
        }

        tallest = forest[forest.len() - 1][j].height;
        for i in (0..forest.len()).rev() {
            forest[i][j].is_visible |= i == forest.len() - 1 || forest[i][j].height > tallest;
            tallest = tallest.max(forest[i][j].height);
        }
    }

    let count = forest
        .iter()
        .flat_map(|trees| trees.iter().map(|t| t.is_visible))
        .filter(|visible| *visible)
        .count();

    forest.iter().for_each(|trees| {
        println!(
            "{}",
            trees
                .iter()
                .map(|t| if t.is_visible {
                    format!("\x1b[91m{}\x1b[0m", t.height)
                } else {
                    format!("{}", t.height)
                })
                .collect::<Vec<_>>()
                .join("")
        )
    });

    println!();
    println!("{count}");

    let mut max_score = 0usize;
    for i in 0..forest.len() {
        for j in 0..forest[i].len() {
            max_score = max_score.max(
                [
                    Direction::Up,
                    Direction::Down,
                    Direction::Left,
                    Direction::Right,
                ]
                .map(|dir| {
                    get_visible_trees(
                        &forest,
                        (i as isize, j as isize) + dir,
                        forest[i][j].height,
                        dir,
                    )
                })
                .iter()
                .product(),
            );
        }
    }

    println!("{}", max_score);
}
