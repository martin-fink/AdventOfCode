use std::{num::ParseIntError, ops::RangeInclusive};

fn main() {
    let lines: Vec<String> = common::read_lines("data/input.txt").expect("File could not be read");

    let result = lines
        .iter()
        .map(|line| parse_ranges(line))
        .map(|r| r.expect("Could not parse"))
        .filter(|(r1, r2)| includes_fully(r1, r2) || includes_fully(r2, r1))
        .count();

    println!("{result}");

    let result = lines
        .iter()
        .map(|line| parse_ranges(line))
        .map(|r| r.expect("Could not parse"))
        .filter(|(r1, r2)| includes_partially(r1, r2) || includes_partially(r2, r1))
        .count();

    println!("{result}");
}

fn includes_fully<T: Ord>(range1: &RangeInclusive<T>, range2: &RangeInclusive<T>) -> bool {
    range1.contains(range2.start()) && range1.contains(range2.end())
}

fn includes_partially<T: Ord>(range1: &RangeInclusive<T>, range2: &RangeInclusive<T>) -> bool {
    range1.contains(range2.start()) || range1.contains(range2.end())
}

fn parse_ranges(line: &str) -> Result<(RangeInclusive<u32>, RangeInclusive<u32>), ParseIntError> {
    let split = line.split(',').collect::<Vec<_>>();

    let l = parse_range(split[0])?;
    let r = parse_range(split[1])?;

    Ok((l, r))
}

fn parse_range(range: &str) -> Result<RangeInclusive<u32>, ParseIntError> {
    let split = range.split('-').collect::<Vec<_>>();

    let (start, end) = (split[0].parse::<u32>()?, split[1].parse::<u32>()?);

    Ok(RangeInclusive::new(start, end))
}
