use anyhow::Result;
use aoc::aoc_main;
use day21::{part1, part2};

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}
