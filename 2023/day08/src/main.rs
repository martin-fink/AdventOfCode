use anyhow::{Context, Result};
use aoc::aoc_main;
use regex::Regex;
use std::collections::HashMap;
use std::hint::unreachable_unchecked;

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

#[derive(Debug, Copy, Clone)]
enum Direction {
    L,
    R,
}

type PathMap<'a> = HashMap<&'a str, (&'a str, &'a str)>;

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

fn parse_line(s: &str) -> Result<(&str, (&str, &str))> {
    let regex = Regex::new("(.+) = \\((.+), (.+)\\)")?;

    let captures = regex.captures(s).context("no captures")?;

    Ok((
        captures.get(1).unwrap().as_str(),
        (
            captures.get(2).unwrap().as_str(),
            captures.get(3).unwrap().as_str(),
        ),
    ))
}

fn shortest_path(directions: &str, maps: &PathMap, start: &str) -> usize {
    let mut steps = 0usize;
    let mut current = start;

    while current.as_bytes()[2] as char != 'Z' {
        let instruction = maps.get(current).unwrap();
        current = match directions.as_bytes()[steps % directions.len()] as char {
            'L' => instruction.0,
            'R' => instruction.1,
            _ => unsafe { unreachable_unchecked() },
        };

        steps += 1;
    }

    steps
}

fn parse_input(s: &str) -> Result<(&str, PathMap)> {
    let mut lines = s.lines();

    let directions = lines.next().context("no first line")?;

    let maps = lines
        .skip(1)
        .flat_map(|line| parse_line(line))
        .collect::<PathMap>();

    Ok((directions, maps))
}

fn part1(s: &str) -> Result<usize> {
    let (directions, maps) = parse_input(s)?;

    Ok(shortest_path(directions, &maps, "AAA"))
}

fn part2(s: &str) -> Result<usize> {
    let (directions, maps) = parse_input(s)?;

    let steps = maps
        .keys()
        .filter(|k| k.ends_with('A'))
        .map(|current| shortest_path(directions, &maps, current))
        .fold(1, num::integer::lcm);

    Ok(steps)
}
