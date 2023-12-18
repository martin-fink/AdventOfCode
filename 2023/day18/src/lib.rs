type Pos = (isize, isize);

#[derive(Copy, Clone)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}

impl Dir {
    fn step(&self, pos: Pos, amount: isize) -> Pos {
        match self {
            Dir::Up => (pos.0 - amount, pos.1),
            Dir::Down => (pos.0 + amount, pos.1),
            Dir::Left => (pos.0, pos.1 - amount),
            Dir::Right => (pos.0, pos.1 + amount),
        }
    }
}

impl TryFrom<char> for Dir {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'U' => Ok(Dir::Up),
            'D' => Ok(Dir::Down),
            'L' => Ok(Dir::Left),
            'R' => Ok(Dir::Right),
            _ => Err(()),
        }
    }
}

struct Instruction {
    dir: Dir,
    length: isize,
}

fn compute_area<F: Fn(&str) -> (Dir, isize)>(s: &str, parse_len: F) -> isize {
    let (nodes, _, perim) = s
        .lines()
        .map(|line| {
            let (dir, length) = parse_len(line);
            Instruction { dir, length }
        })
        .fold((Vec::new(), (0, 0), 0), |(mut nodes, pos, perim), inst| {
            let next_pos = inst.dir.step(pos, inst.length);
            nodes.push(next_pos);

            (nodes, next_pos, perim + inst.length)
        });

    // shoelace formula
    let n = nodes.len();
    let mut a = 0;
    for i in 0..n - 1 {
        a += nodes[i].0 * nodes[i + 1].1 - nodes[i + 1].0 * nodes[i].1;
    }
    (a + nodes[n - 1].0 * nodes[0].1 - nodes[0].0 * nodes[n - 1].1).abs() / 2 + perim / 2 + 1
}

pub fn part1(s: &str) -> isize {
    compute_area(s, |line| {
        let mut splits = line.split(' ');

        (
            Dir::try_from(splits.next().unwrap().chars().next().unwrap()).unwrap(),
            splits.next().unwrap().parse().unwrap(),
        )
    })
}

pub fn part2(s: &str) -> isize {
    compute_area(s, |line| {
        let idx = line.find('#').unwrap();
        let dir = match line.as_bytes()[line.len() - 2] as char {
            '0' => Dir::Right,
            '1' => Dir::Down,
            '2' => Dir::Left,
            '3' => Dir::Up,
            _ => unreachable!(),
        };
        (
            dir,
            isize::from_str_radix(&line[idx + 1..line.len() - 2], 16).unwrap(),
        )
    })
}

#[cfg(test)]
mod tests {
    use crate::{part1, part2};

    #[test]
    fn p1() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part1(input), 62365);
    }

    #[test]
    fn p2() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part2(input), 159485361249806);
    }
}
