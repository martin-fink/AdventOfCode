use anyhow::Result;
use aoc::aoc_main;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum Direction {
    Up,
    Right,
    Down,
    Left,
}

fn roll_stones(s: &mut str, direction: Direction) {
    let width = (s.find('\n').unwrap() + 1) as isize;
    let lines = (s.len() as isize + 1) / width;

    let roll_stone = |bytes: &mut [u8], row: isize, col: isize| {
        // SAFETY: string always stays utf-8
        let mut index = row * width + col;
        let offset = match direction {
            Direction::Up => -width,
            Direction::Right => 1,
            Direction::Down => width,
            Direction::Left => -1,
        };
        while (0..bytes.len() as isize).contains(&(index))
            && (0..bytes.len() as isize).contains(&(index + offset))
        {
            let idx = (index + offset) as usize;
            if bytes[idx] as char != '.' {
                return;
            }
            bytes.swap(idx, index as usize);
            index += offset;
        }
    };

    let bytes = unsafe { s.as_bytes_mut() };
    match direction {
        Direction::Up | Direction::Left => {
            for row in 0..lines {
                for col in 0..width - 1 {
                    if bytes[(row * width + col) as usize] as char == 'O' {
                        roll_stone(bytes, row, col);
                    }
                }
            }
        }
        Direction::Right => {
            for row in 0..lines {
                for col in (0..width - 1).rev() {
                    if bytes[(row * width + col) as usize] as char == 'O' {
                        roll_stone(bytes, row, col);
                    }
                }
            }
        }
        Direction::Down => {
            for row in (0..lines).rev() {
                for col in 0..width - 1 {
                    if bytes[(row * width + col) as usize] as char == 'O' {
                        roll_stone(bytes, row, col);
                    }
                }
            }
        }
    }
}

fn part1(s: &str) -> usize {
    let mut s = s.to_string();
    let width = s.find('\n').unwrap() + 1;
    let lines = (s.len() + 1) / width;
    roll_stones(&mut s, Direction::Up);

    s.lines()
        .enumerate()
        .map(|(n, line)| line.bytes().filter(|b| *b as char == 'O').count() * (lines - n))
        .sum()
}

fn part2(s: &str) -> usize {
    let mut s = s.to_string();
    let width = s.find('\n').unwrap() + 1;
    let lines = (s.len() + 1) / width;
    let mut cache: HashMap<String, usize> = Default::default();
    let mut i = 0;
    const END: usize = 1_000_000_000;
    while i < END {
        let entry = cache.entry(s.to_string());
        match entry {
            Entry::Occupied(entry) => {
                let cycle_len = i - *entry.get();
                i = END - (END - i) % cycle_len;
                cache.clear();
            }
            Entry::Vacant(entry) => {
                entry.insert(i);
            }
        }
        roll_stones(&mut s, Direction::Up);
        roll_stones(&mut s, Direction::Left);
        roll_stones(&mut s, Direction::Down);
        roll_stones(&mut s, Direction::Right);
        i += 1;
    }

    let result = s
        .lines()
        .enumerate()
        .map(|(n, line)| line.bytes().filter(|b| *b as char == 'O').count() * (lines - n))
        .sum();

    result
}
