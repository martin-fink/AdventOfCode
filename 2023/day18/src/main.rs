use anyhow::Result;
use aoc::aoc_main;
use day18::{part1, part2};

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}
