use rayon::iter::ParallelIterator;
use std::fs::File;
use anyhow::{Context, Result};
use std::hint::{black_box, unreachable_unchecked};
use std::io::Read;
use std::mem::MaybeUninit;
use rayon::prelude::IntoParallelRefIterator;

fn main() -> Result<()> {
    let path = std::env::args().skip(2).next().unwrap();

    let file = File::open(&path).unwrap();
    let mut buf_reader = std::io::BufReader::new(file);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content)?;

    let mut result = 0;
    // for _ in 0..1000 {
        result = if std::env::args().skip(1).next().unwrap() == "part1" {
            black_box(part1(&content))
        } else {
            black_box(part2(&content))
        }?;
    // }

    println!("{result}");

    Ok(())
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    L,
    R,
}

// 26^3 = 17576
type PathMap<'a> = [MaybeUninit<(&'a str, &'a str)>; 262144];

#[inline]
fn hash_letters_str(letters: &str) -> usize {
    debug_assert!(letters.len() == 3);
    let bytes = letters.as_bytes();
    (((bytes[0] - 0x41) as usize) << 12)
        | (((bytes[1] - 0x41) as usize) << 6)
        | ((bytes[2] - 0x41) as usize)
}

impl TryFrom<char> for Direction {
    type Error = ();

    fn try_from(value: char) -> std::result::Result<Self, Self::Error> {
        match value {
            'L' => Ok(Self::L),
            'R' => Ok(Self::R),
            _ => Err(()),
        }
    }
}

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
    let mut maps = Box::new([MaybeUninit::uninit(); 262144]);

    lines
        .skip(1)
        .map(|line| parse_line(line))
        .for_each(|(k, v)| {
            maps[hash_letters_str(k)] = MaybeUninit::new(v);
            if k.as_bytes()[2] as char == 'A' {
                beginnings.push(k);
            }
        });

    Ok((directions, beginnings, maps))
}

fn part1(s: &str) -> Result<usize> {
    let (directions, _, maps) = parse_input(s)?;

    Ok(shortest_path(directions, &maps, "AAA"))
}

fn part2(s: &str) -> Result<usize> {
    let (directions, beginnings, maps) = parse_input(s)?;

    let steps = beginnings
        .par_iter()
        // .iter()
        .filter_map(|k| if k.as_bytes()[2] as char == 'A' { Some(shortest_path(directions, &maps, k)) } else { None })
        .collect::<Vec<_>>()
        .iter()
        .cloned()
        .fold(1, num::integer::lcm);

    Ok(steps)
}
