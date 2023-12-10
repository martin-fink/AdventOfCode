use anyhow::{Context, Result};
use std::alloc;
use std::alloc::Layout;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::hint::black_box;
use std::io::Read;
use std::mem::MaybeUninit;

fn read_file(path: &str) -> Result<String> {
    let mut file = File::open(path).with_context(|| format!("Failed to open {}", path))?;
    let mut content = String::new();
    file.read_to_string(&mut content)?;
    Ok(content)
}

fn main() -> Result<()> {
    let mut args = std::env::args().skip(1);
    let p1 = args.next().context("Usage: ./program part1|part2 path")? == "part1";
    let file = args.next().context("Usage: ./program part1|part2 path")?;

    let input = read_file(&file)?;
    let result = black_box(if p1 { part1(&input) } else { part2(&input) }?);

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
                let mut neighbours = [MaybeUninit::uninit(); 2];
                let mut idx = 0;

                if pos.0 > 0
                    && matches!(
                        self.get((pos.0 - 1, pos.1)),
                        GridElement::Vertical | GridElement::LeftTop | GridElement::RightTop
                    )
                {
                    neighbours[idx] = MaybeUninit::new((pos.0 - 1, pos.1));
                    idx += 1
                }
                if pos.1 > 0
                    && matches!(
                        self.get((pos.0, pos.1 - 1)),
                        GridElement::Horizontal | GridElement::LeftTop | GridElement::LeftBot
                    )
                {
                    neighbours[idx] = MaybeUninit::new((pos.0, pos.1 - 1));
                    idx += 1
                }
                let len = (self.input.len() + 1) / self.width;
                if pos.0 < len
                    && matches!(
                        self.get((pos.0 + 1, pos.1)),
                        GridElement::Vertical | GridElement::RightBot | GridElement::LeftBot
                    )
                {
                    neighbours[idx] = MaybeUninit::new((pos.0 - 2, pos.1));
                    idx += 1
                }
                if pos.1 > self.width
                    && matches!(
                        self.get((pos.0, pos.1 + 1)),
                        GridElement::Horizontal | GridElement::RightTop | GridElement::RightBot
                    )
                {
                    neighbours[idx] = MaybeUninit::new((pos.0, pos.1 - 2));
                }

                Some(unsafe { (neighbours[0].assume_init(), neighbours[1].assume_init()) })
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

    fn get_loop(&self) -> Box<[bool]> {
        let mut path = alloc_box_buffer(self.input.len());

        let mut prev = None;
        let mut current = self.start;
        loop {
            let index = current.0 * self.width + current.1;
            if path[index] {
                // we are now at the start
                debug_assert_eq!(current, self.start);
                break;
            }
            path[index] = true;

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

fn alloc_box_buffer(len: usize) -> Box<[bool]> {
    debug_assert_ne!(len, 0);
    let layout = Layout::array::<bool>(len * std::mem::size_of::<bool>()).unwrap();
    let ptr = unsafe { alloc::alloc_zeroed(layout) }.cast();
    let slice_ptr = core::ptr::slice_from_raw_parts_mut(ptr, len);
    unsafe { Box::from_raw(slice_ptr) }
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
            if l[x * grid.width + y] {
                let moves_down = x > 0 && l[(x - 1) * grid.width + y];
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
