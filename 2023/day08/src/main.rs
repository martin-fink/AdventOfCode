use anyhow::{Context, Result};
use aoc::aoc_main;
use std::collections::HashMap;

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

fn parse_directions(s: &str) -> Vec<Direction> {
    s.chars().flat_map(|c| c.try_into()).collect()
}

#[inline]
fn parse_line(s: &str) -> (&str, (&str, &str)) {
    (&s[0..3], (&s[7..10], &s[12..15]))
}

fn shortest_path(
    directions: &[Direction],
    maps: &PathMap,
    start: &str,
) -> usize {
    let mut steps = 0usize;
    let mut current = start;

    while !current.ends_with('Z') {
        let instruction = maps.get(current).unwrap();
        current = match directions[steps % directions.len()] {
            Direction::L => instruction.0,
            Direction::R => instruction.1,
        };

        steps += 1;
    }

    steps
}


fn parse_input(s: &str) -> Result<(Vec<Direction>, PathMap)> {
    let mut lines = s.lines();

    let directions = parse_directions(lines.next().context("no first line")?);

    let maps = lines
        .skip(1)
        .map(|line| parse_line(line))
        .collect::<PathMap>();

    Ok((directions, maps))
}

fn part1(s: &str) -> Result<usize> {
    let (directions, maps) = parse_input(s)?;

    Ok(shortest_path(&directions, &maps, "AAA"))
}

fn part2(s: &str) -> Result<usize> {
    let (directions, maps) = parse_input(s)?;

    let steps = maps
        .keys()
        .filter(|k| k.ends_with('A'))
        .map(|current| shortest_path(&directions, &maps, current))
        .fold(1, num::integer::lcm);

    Ok(steps)
}
