trait IsEven {
    fn is_even(&self) -> bool;
}

impl IsEven for usize {
    fn is_even(&self) -> bool {
        self % 2 == 0
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct Pos {
    row: usize,
    col: usize,
}

fn try_step(pos: Pos, r: isize, c: isize, width: usize, height: usize) -> Option<Pos> {
    let new_r = pos.row as isize + r;
    let new_c = pos.col as isize + c;

    if new_r < 0 || new_c < 0 || new_r >= height as isize || new_c >= width as isize {
        None
    } else {
        Some(Pos {
            row: new_r as usize,
            col: new_c as usize,
        })
    }
}

fn count_locations(s: &str, initial: Pos, steps: usize, even: bool) -> (usize, Vec<Option<bool>>) {
    let width = s.find('\n').unwrap();
    let height = (s.len() + 1) / (width + 1);

    let mut seen: Vec<Option<bool>> = vec![None; width * height];
    // let mut seen: HashMap<Pos, bool> = HashMap::new();
    let mut current = vec![initial];

    current.iter().for_each(|pos| {
        // seen.insert(*pos, true);
        seen[pos.row * width + pos.col] = Some(true);
    });

    for step in 1..=steps {
        let mut next_pos = Vec::new();
        for pos in current {
            [(1, 0), (-1, 0), (0, 1), (0, -1)]
                .into_iter()
                .filter_map(|(r, c)| try_step(pos, r, c, width, height))
                .filter(|pos| s.as_bytes()[pos.row * (width + 1) + pos.col] as char != '#')
                .for_each(|pos| match &mut seen[pos.row * width + pos.col] {
                    Some(_) => {}
                    e => {
                        *e = Some(step.is_even());
                        next_pos.push(pos);
                    }
                });
        }

        current = next_pos;
    }

    (
        seen.iter()
            .filter(|is_even| **is_even == Some(even))
            .count(),
        seen,
    )
}

pub fn part1(s: &str) -> usize {
    count_locations(s, find_start(s), 64, 64.is_even()).0
}

fn locations_inf_grid(s: &str, total_steps: usize) -> usize {
    // interesting note: the paths left, right, up, and down of the starting position are always free!
    // first we need to find out how many locations are reachable with an odd number of steps
    let width = s.find('\n').unwrap();
    let height = (s.len() + 1) / (width + 1);

    let strictly_in_corner = |r, c| {
        usize::min(height - 1 - r, r) + c < (height - 1) / 2
            || usize::max(height - 1 - r, r) + c > 3 * (height - 1) / 2
    };
    let in_corner = |r, c| {
        usize::min(height - 1 - r, r) + c <= (height - 1) / 2
            || usize::max(height - 1 - r, r) + c >= 3 * (height - 1) / 2
    };

    let starting_pos = find_start(s);

    let (center, other, seen) = {
        let (full_rect_even, seen) = count_locations(
            s,
            starting_pos,
            usize::min(width * 2 + 1, total_steps),
            true,
        );
        let (full_rect_odd, _) = count_locations(
            s,
            starting_pos,
            usize::min(width * 2 + 1, total_steps),
            false,
        );
        if total_steps.is_even() {
            (full_rect_even, full_rect_odd, seen)
        } else {
            (full_rect_odd, full_rect_even, seen)
        }
    };

    let middle = starting_pos.col;
    let q = (total_steps - middle) / width;

    let mut result = center * (1 + 4 * (2..).step_by(2).take_while(|k| *k < q).sum::<usize>())
        + other * 4 * (1..).step_by(2).take_while(|k| *k < q).sum::<usize>();

    for row in 0..height {
        for col in 0..width {
            if let Some(even) = seen[row * width + col] {
                result += if even == q.is_even() {
                    if in_corner(row, col) {
                        q
                    } else {
                        0
                    }
                } else if strictly_in_corner(row, col) {
                    3 * (q - 1) + 2
                } else {
                    4 * (q - 1) + 4
                }
            }
        }
    }

    result
}

fn find_start(s: &str) -> Pos {
    let width = s.find('\n').unwrap();
    let pos = s.find('S').unwrap();
    let row = pos / (width + 1);
    let col = pos % (width + 1);

    Pos { row, col }
}

pub fn part2(s: &str) -> usize {
    locations_inf_grid(s, 26501365)
}

#[cfg(test)]
mod tests {
    use crate::{part1, part2};

    #[test]
    fn p1() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part1(input), 3733);
    }

    #[test]
    fn p2() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part2(input), 617729401414635);
    }
}
