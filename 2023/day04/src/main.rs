use aoc::aoc_main;
use lazy_static::lazy_static;
use regex::Regex;
use std::collections::HashSet;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

lazy_static! {
    static ref SCRATCH_CARD_REGEX: Regex =
        Regex::new("^Card\\s+\\d+:\\s([\\s\\d]+)\\s+\\|\\s+([\\s\\d]+)").unwrap();
}

fn part1(s: &str) -> anyhow::Result<u32> {
    let result = s
        .lines()
        .map(get_winners)
        .map(|winners| {
            if winners > 0 {
                1u32 << (winners - 1)
            } else {
                0
            }
        })
        .sum();

    Ok(result)
}

fn parse_nums<T>(s: &str) -> Option<T>
where
    T: FromIterator<u32>,
{
    let num_regex = Regex::new("\\d+").unwrap();

    let result = num_regex
        .captures_iter(s)
        .map(|n| n.get(0).unwrap().as_str().parse().unwrap())
        .collect();

    Some(result)
}

fn get_winners(line: &str) -> usize {
    let m = SCRATCH_CARD_REGEX.captures(line).unwrap();
    let winners: HashSet<u32> = parse_nums(m.get(1).unwrap().as_str()).unwrap();
    let nums: Vec<u32> = parse_nums(m.get(2).unwrap().as_str()).unwrap();

    nums.iter().flat_map(|num| winners.get(num)).count()
}

fn part2(s: &str) -> anyhow::Result<u32> {
    let winners = s.lines().map(get_winners).collect::<Vec<_>>();

    let mut copies = vec![1usize; winners.len()];

    for i in 0..winners.len() {
        let n = winners[i];
        for j in i + 1..usize::min(i + n + 1, winners.len()) {
            copies[j] += copies[i]
        }
    }

    Ok(copies.iter().sum::<usize>() as u32)
}
