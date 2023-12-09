use anyhow::Result;
use aoc::aoc_main;
use itertools::Itertools;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

fn part1(s: &str) -> Result<i32> {
    Ok(s.lines()
        .map(parse_line)
        .map(predict)
        .map(|(_, next)| next)
        .sum())
}

fn part2(s: &str) -> Result<i32> {
    Ok(s.lines()
        .map(parse_line)
        .map(predict)
        .map(|(last, _)| last)
        .sum())
}

fn predict(nums: Vec<i32>) -> (i32, i32) {
    let mut all_zeroes = false;
    let mut stack = vec![nums];
    while !all_zeroes {
        all_zeroes = true;
        let current = stack.last().unwrap();
        stack.push(
            current
                .iter()
                .tuple_windows()
                .map(|(a, b)| {
                    if *a != 0 && *b != 0 {
                        all_zeroes = false
                    }
                    b - a
                })
                .collect(),
        )
    }

    // merging all
    stack
        .iter()
        .rev()
        .map(|vec| (*vec.first().unwrap(), *vec.last().unwrap()))
        .fold((0, 0), |acc, i| (i.0 - acc.0, i.1 + acc.1))
}

fn parse_line(s: &str) -> Vec<i32> {
    s.split(' ')
        .map(|num| num.parse::<i32>().unwrap())
        .collect()
}
