use anyhow::{Context, Result};
use aoc::aoc_main;
use colored::Colorize;
use std::collections::{HashSet, VecDeque};
use std::fmt::{Display, Formatter};

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

#[derive(Copy, Clone, Eq, PartialEq)]
enum GridElement {
    Ground,
    Vertical,
    Horizontal,
    LeftTop,
    RightTop,
    LeftBot,
    RightBot,
    Start,
}

struct Grid {
    elems: Vec<Vec<GridElement>>,
}

type Pos = (usize, usize);

impl Display for Grid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for row in &self.elems {
            for item in row {
                write!(f, "{}", item)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Grid {
    fn get(&self, pos: Pos) -> GridElement {
        self.elems[pos.0][pos.1]
    }

    fn get_connecting_points(&self, pos: Pos) -> Option<(Pos, Pos)> {
        let elem = self.get(pos);

        let checked_add_x = |x, n| {
            if x + n >= self.elems.len() {
                None
            } else {
                Some(x + n)
            }
        };
        let checked_add_y = |y, n| {
            if y + n >= self.elems.first()?.len() {
                None
            } else {
                Some(y + n)
            }
        };

        match elem {
            GridElement::Ground => None,
            GridElement::Vertical => Some((
                (pos.0.checked_sub(1)?, pos.1),
                (checked_add_x(pos.0, 1)?, pos.1),
            )),
            GridElement::Horizontal => Some((
                (pos.0, pos.1.checked_sub(1)?),
                (pos.0, checked_add_y(pos.1, 1)?),
            )),
            GridElement::LeftTop => Some((
                (checked_add_x(pos.0, 1)?, pos.1),
                (pos.0, checked_add_y(pos.1, 1)?),
            )),
            GridElement::RightTop => Some((
                (checked_add_x(pos.0, 1)?, pos.1),
                (pos.0, pos.1.checked_sub(1)?),
            )),
            GridElement::LeftBot => Some((
                (pos.0.checked_sub(1)?, pos.1),
                (pos.0, checked_add_y(pos.1, 1)?),
            )),
            GridElement::RightBot => Some((
                (pos.0.checked_sub(1)?, pos.1),
                (pos.0, pos.1.checked_sub(1)?),
            )),
            GridElement::Start => {
                // first get all neighbours
                let mut vec = Vec::with_capacity(8);
                for x in pos.0.saturating_sub(1)..=usize::min(pos.0 + 1, self.elems.len() - 1) {
                    for y in
                        pos.1.saturating_sub(1)..=usize::min(pos.1 + 1, self.elems[x].len() - 1)
                    {
                        if x != pos.0 || y != pos.1 {
                            vec.push((x, y))
                        }
                    }
                }

                let filtered = vec
                    .iter()
                    .filter(|p| {
                        let Some((n1, n2)) = self.get_connecting_points(**p) else {
                            return false;
                        };
                        n1 == pos || n2 == pos
                    })
                    .collect::<Vec<_>>();

                assert_eq!(filtered.len(), 2);

                Some((*filtered[0], *filtered[1]))
            }
        }
    }
}

impl TryFrom<char> for GridElement {
    type Error = ();

    fn try_from(value: char) -> std::result::Result<Self, Self::Error> {
        match value {
            '|' => Ok(Self::Vertical),
            '-' => Ok(Self::Horizontal),
            'L' => Ok(Self::LeftBot),
            'J' => Ok(Self::RightBot),
            '7' => Ok(Self::RightTop),
            'F' => Ok(Self::LeftTop),
            '.' => Ok(Self::Ground),
            'S' => Ok(Self::Start),
            _ => Err(()),
        }
    }
}

impl Display for GridElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let ch = match self {
            GridElement::Ground => '.',
            GridElement::Vertical => '│',
            GridElement::Horizontal => '─',
            GridElement::LeftTop => '┌',
            GridElement::RightTop => '┐',
            GridElement::LeftBot => '└',
            GridElement::RightBot => '┘',
            GridElement::Start => 'S',
        };
        write!(f, "{}", ch)
    }
}

fn parse_input(s: &str) -> Grid {
    Grid {
        elems: s
            .lines()
            .map(|line| line.chars().flat_map(GridElement::try_from).collect())
            .collect(),
    }
}

fn get_farthest_point(grid: Grid, start: Pos) -> u32 {
    let mut seen = HashSet::new();

    let mut worklist = VecDeque::new();
    worklist.push_back((start, 0));
    let mut max = 0;

    while let Some((elem, distance)) = worklist.pop_front() {
        if !seen.insert(elem) {
            continue;
        }

        max = u32::max(distance, max);
        let (n1, n2) = grid
            .get_connecting_points(elem)
            .expect("we should not be looking at ground");
        worklist.push_back((n1, distance + 1));
        worklist.push_back((n2, distance + 1));
    }

    max
}

fn get_start(grid: &Grid) -> Result<Pos> {
    grid.elems
        .iter()
        .enumerate()
        .filter_map(|(x, grid)| {
            grid.iter()
                .enumerate()
                .filter_map(|(y, elem)| {
                    if *elem == GridElement::Start {
                        Some((x, y))
                    } else {
                        None
                    }
                })
                .next()
        })
        .next()
        .context("could not find start pos")
}

fn part1(s: &str) -> Result<u32> {
    let grid = parse_input(s);
    println!("{grid}");

    let start = get_start(&grid)?;
    Ok(get_farthest_point(grid, start))
}

fn part2(s: &str) -> Result<u32> {
    let grid = parse_input(s);

    let start = get_start(&grid)?;

    let mut seen = HashSet::new();
    let mut worklist = VecDeque::new();
    worklist.push_back(start);
    while let Some(elem) = worklist.pop_front() {
        if !seen.insert(elem) {
            continue;
        }

        let (n1, n2) = grid
            .get_connecting_points(elem)
            .context("we should not be looking at ground")?;
        worklist.push_back(n1);
        worklist.push_back(n2);
    }

    let mut area = 0;
    for (x, row) in grid.elems.iter().enumerate() {
        let mut in_grid = false;
        for (y, elem) in row.iter().enumerate() {
            if seen.get(&(x, y)).is_some() {
                let moves_down = x > 0 && seen.get(&(x - 1, y)).is_some();
                if moves_down {
                    let (n1, n2) = grid.get_connecting_points((x - 1, y)).unwrap();
                    if n1 == (x, y) || n2 == (x, y) {
                        in_grid = !in_grid;
                    }
                }
                print!("{}", format!("{}", elem).green())
            } else if in_grid {
                area += 1;
                print!("{}", "▒".blue());
            } else {
                print!("{}", elem);
            }
        }
        println!()
    }

    Ok(area)
}
