use num::Integer;


#[derive(Copy, Clone, Eq, PartialEq)]
struct Pos {
    row: usize,
    col: usize,
}

fn try_step(pos: Pos, r: isize, c: isize, width: usize, height: usize) -> Option<Pos> {
    let new_r = pos.row as isize + r;
    let new_c = pos.col as isize + c;

    if new_r < 0 || new_c < 0 || new_r as usize >= height || new_c as usize >= width {
        None
    } else {
        Some(Pos {
            row: new_r as usize,
            col: new_c as usize,
        })
    }
}

pub fn part1(s: &str) -> usize {
    let width = s.find('\n').unwrap();
    let height = (s.len() + 1) / (width + 1);

    let starting_pos = {
        let pos = s.find('S').unwrap();
        let row = pos / (width + 1);
        let col = pos % (width + 1);

        Pos { row, col }
    };

    let mut seen: Vec<Option<usize>> = vec![None; width * height];
    let mut current = vec![starting_pos];

    seen[starting_pos.row * width + starting_pos.col] = Some(0);

    let steps = 64;
    for step in 1..=steps {
        let mut next_pos = Vec::new();
        for pos in current {
            [(1, 0), (-1, 0), (0, 1), (0, -1)]
                .into_iter()
                .filter_map(|(r, c)| try_step(pos, r, c, width, height))
                .filter(|pos| s.as_bytes()[pos.row * (width + 1) + pos.col] as char == '.')
                .for_each(|pos| match &mut seen[pos.row * width + pos.col] {
                    Some(_) => {}
                    val => {
                        *val = Some(step);
                        next_pos.push(pos);
                    }
                });
        }

        current = next_pos;
    }

    seen.iter()
        .flat_map(|seen| *seen)
        .filter(|seen| seen.is_even() == steps.is_even())
        .count()
}

pub fn part2(_s: &str) -> usize {
    todo!()
}

#[cfg(test)]
mod tests {
    use crate::{part1};

    #[test]
    fn p1() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part1(input), 3733);
    }

    // #[test]
    // fn p2() {
    //     let input = include_str!("../input/input.txt");
    //     assert_eq!(part2(input), 232605773145467);
    // }
}
