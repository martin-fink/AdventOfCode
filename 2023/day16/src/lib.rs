enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    #[inline]
    fn move_forward(&self, pos: (isize, isize)) -> (isize, isize) {
        match self {
            Dir::Up => (pos.0 - 1, pos.1),
            Dir::Down => (pos.0 + 1, pos.1),
            Dir::Left => (pos.0, pos.1 - 1),
            Dir::Right => (pos.0, pos.1 + 1),
        }
    }

    #[inline]
    fn bitflag(&self) -> u8 {
        match self {
            Dir::Up => 0b0001,
            Dir::Down => 0b0010,
            Dir::Left => 0b0100,
            Dir::Right => 0b1000,
        }
    }
}

fn get_energized_tiles(s: &str, start: (isize, isize), initial_dir: Dir) -> usize {
    let width = s.find('\n').unwrap();
    let height = (s.len() + 1) / (width + 1);

    let mut energized = vec![0u8; width * height];

    let mut worklist = Vec::new();
    worklist.push((start, initial_dir));

    let mut tiles = 0;

    while let Some(((row, col), dir)) = worklist.pop() {
        if col >= width as isize || row >= height as isize || col < 0 || row < 0 {
            continue;
        }

        let entry = energized
            .get_mut(row as usize * width + col as usize)
            .unwrap();
        if *entry & dir.bitflag() != 0 {
            continue;
        }
        if *entry != 0 {
            tiles += 1;
        }

        *entry |= dir.bitflag();

        match s.as_bytes()[row as usize * (width + 1) + col as usize] as char {
            '/' => {
                let new_dir = match dir {
                    Dir::Up => Dir::Right,
                    Dir::Down => Dir::Left,
                    Dir::Left => Dir::Down,
                    Dir::Right => Dir::Up,
                };
                worklist.push((new_dir.move_forward((row, col)), new_dir));
            }
            '\\' => {
                let new_dir = match dir {
                    Dir::Up => Dir::Left,
                    Dir::Down => Dir::Right,
                    Dir::Left => Dir::Up,
                    Dir::Right => Dir::Down,
                };
                worklist.push((new_dir.move_forward((row, col)), new_dir));
            }
            '-' if matches!(dir, Dir::Up | Dir::Down) => {
                worklist.push((Dir::Left.move_forward((row, col)), Dir::Left));
                worklist.push((Dir::Right.move_forward((row, col)), Dir::Right));
            }
            '|' if matches!(dir, Dir::Left | Dir::Right) => {
                worklist.push((Dir::Up.move_forward((row, col)), Dir::Up));
                worklist.push((Dir::Down.move_forward((row, col)), Dir::Down));
            }
            '.' | '-' | '|' => worklist.push((dir.move_forward((row, col)), dir)),
            _ => unreachable!(),
        }
    }

    tiles
}

pub fn part1(s: &str) -> usize {
    get_energized_tiles(s, (0, 0), Dir::Right)
}

pub fn part2(s: &str) -> usize {
    let width = s.find('\n').unwrap();
    let height = (s.len() + 1) / (width + 1);

    let mut max = 0;

    for i in 0..height {
        max = usize::max(max, get_energized_tiles(s, (i as isize, 0), Dir::Right));
        max = usize::max(
            max,
            get_energized_tiles(s, (i as isize, width as isize - 1), Dir::Left),
        );
    }
    for i in 0..width {
        max = usize::max(max, get_energized_tiles(s, (0, i as isize), Dir::Down));
        max = usize::max(
            max,
            get_energized_tiles(s, (height as isize - 1, i as isize), Dir::Up),
        );
    }

    max
}
