use anyhow::{Context, Result};
use std::fs::File;
use std::hint::{black_box, unreachable_unchecked};
use std::io::Read;
use std::mem::MaybeUninit;

fn main() -> Result<()> {
    let path = std::env::args().nth(2).unwrap();

    let file = File::open(path).unwrap();
    let mut buf_reader = std::io::BufReader::new(file);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content)?;

    let result = if std::env::args().nth(1).unwrap() == "part1" {
        black_box(part1(&content))
    } else {
        black_box(part2(&content))
    }?;

    println!("{result}");

    Ok(())
}

// 26^3 = 17576
type PathMap<'a> = [MaybeUninit<(&'a str, &'a str)>; 32 * 32 * 32];

#[inline]
fn hash_letters_str(letters: &str) -> usize {
    debug_assert!(letters.len() == 3);
    let bytes = letters.as_bytes();
    (((bytes[0] - 0x41) as usize) * 32 * 32)
        | (((bytes[1] - 0x41) as usize) * 32)
        | ((bytes[2] - 0x41) as usize)
}

#[inline]
fn parse_line(s: &str) -> (&str, (&str, &str)) {
    (&s[0..3], (&s[7..10], &s[12..15]))
}

fn shortest_path(directions: &str, maps: &PathMap, start: &str) -> usize {
    let mut steps = 0usize;
    let mut current = start;

    while current.as_bytes()[2] as char != 'Z' {
        let instruction = unsafe { maps[hash_letters_str(current)].assume_init() };
        current = match directions.as_bytes()[steps % directions.len()] as char {
            'L' => instruction.0,
            'R' => instruction.1,
            _ => unsafe { unreachable_unchecked() },
        };

        steps += 1;
    }

    steps
}

fn parse_input(s: &str) -> Result<(&str, Vec<&str>, Box<PathMap>)> {
    let mut lines = s.lines();

    let directions = lines.next().context("no first line")?;

    let mut beginnings = Vec::new();
    let mut maps = Box::new([MaybeUninit::uninit(); 32 * 32 * 32]);

    lines.skip(1).map(parse_line).for_each(|(k, v)| {
        maps[hash_letters_str(k)] = MaybeUninit::new(v);
        if k.as_bytes()[2] as char == 'A' {
            beginnings.push(k);
        }
    });

    Ok((directions, beginnings, maps))
}

#[inline]
fn part1(s: &str) -> Result<usize> {
    let (directions, _, maps) = parse_input(s)?;

    Ok(shortest_path(directions, &maps, "AAA"))
}

#[inline]
fn part2(s: &str) -> Result<usize> {
    let (directions, beginnings, maps) = parse_input(s)?;

    let steps = beginnings
        .iter()
        .map(|k| shortest_path(directions, &maps, k))
        .fold(1, num::integer::lcm);

    Ok(steps)
}
