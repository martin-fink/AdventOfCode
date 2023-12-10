use anyhow::{Context, Result};
use aoc::aoc_main;
use std::collections::HashSet;
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

struct Grid<'a> {
    input: &'a str,
    width: usize,
    start: Pos,
}

type Pos = (usize, usize);

impl<'a> Display for Grid<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.input)?;
        Ok(())
    }
}

impl<'a> Grid<'a> {
    fn new(input: &'a str, width: usize) -> Result<Self> {
        let start = Self::get_start(input, width)?;
        Ok(Self {
            input,
            width,
            start,
        })
    }

    #[inline]
    fn get(&self, pos: Pos) -> GridElement {
        GridElement::from(self.input.as_bytes()[pos.0 * self.width + pos.1] as char)
    }

    fn get_connecting_points(&self, pos: Pos) -> Option<(Pos, Pos)> {
        let elem = self.get(pos);

        let checked_add_x = |x, n| {
            if x + n >= self.input.len() {
                None
            } else {
                Some(x + n)
            }
        };
        let checked_add_y = |y, n| {
            if y + n >= self.width - 1 {
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
                for x in pos.0.saturating_sub(1)..=usize::min(pos.0 + 1, self.width - 2) {
                    for y in pos.1.saturating_sub(1)..=usize::min(pos.1 + 1, self.width - 2) {
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

    fn get_start(elems: &str, width: usize) -> Result<Pos> {
        elems
            .bytes()
            .enumerate()
            .filter_map(|(x, elem)| {
                if elem as char == 'S' {
                    Some((x / width, x % width))
                } else {
                    None
                }
            })
            .next()
            .context("could not find start pos")
    }

    fn get_loop(&self) -> HashSet<Pos> {
        let mut path = HashSet::new();

        let mut prev = None;
        let mut current = self.start;
        loop {
            if !path.insert(current) {
                // we are now at the start
                assert_eq!(current, self.start);
                break;
            }

            let (n1, n2) = self
                .get_connecting_points(current)
                .expect("we should not be looking at ground");
            let prev_tmp = Some(current);
            if Some(n1) != prev {
                current = n1;
            } else if Some(n2) != prev {
                current = n2;
            } else {
                unreachable!();
            }
            prev = prev_tmp;
        }

        path
    }
}

impl From<char> for GridElement {
    fn from(value: char) -> Self {
        match value {
            '|' => Self::Vertical,
            '-' => Self::Horizontal,
            'L' => Self::LeftBot,
            'J' => Self::RightBot,
            '7' => Self::RightTop,
            'F' => Self::LeftTop,
            '.' => Self::Ground,
            'S' => Self::Start,
            _ => unreachable!(),
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

fn parse_input(s: &str) -> Result<Grid> {
    let width = s
        .bytes()
        .enumerate()
        .filter_map(|(i, c)| if c as char == '\n' { Some(i + 1) } else { None })
        .next()
        .unwrap();
    Grid::new(s, width)
}

fn part1(s: &str) -> Result<usize> {
    let grid = parse_input(s)?;

    Ok(grid.get_loop().len() / 2)
}

fn part2(s: &str) -> Result<usize> {
    let grid = parse_input(s)?;

    let l = grid.get_loop();

    let mut area = 0;
    for x in 0..(grid.input.len() + 1) / grid.width {
        let mut in_grid = false;
        for y in 0..(grid.width - 1) {
            if l.get(&(x, y)).is_some() {
                let moves_down = x > 0 && l.get(&(x - 1, y)).is_some();
                if moves_down {
                    let (n1, n2) = grid.get_connecting_points((x - 1, y)).unwrap();
                    if n1 == (x, y) || n2 == (x, y) {
                        in_grid = !in_grid;
                    }
                }
            } else if in_grid {
                area += 1;
            }
        }
    }

    Ok(area)
}
