use anyhow::{Context, Result};
use clap::Parser;
use std::fs::File;
use std::io::Read;

#[derive(Parser, Debug)]
enum AocCommand {
    Part1(IOArgs),
    Part2(IOArgs),
}

#[derive(clap::Args, Debug, Clone)]
struct IOArgs {
    /// The input file
    input_path: std::path::PathBuf,
    /// Optional output file to store the result in
    #[arg(long)]
    output: Option<std::path::PathBuf>,
}

fn read_file(path: std::path::PathBuf) -> Result<String> {
    let file = File::open(&path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut buf_reader = std::io::BufReader::new(file);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content)?;
    Ok(content)
}

fn main() -> Result<()> {
    let command = AocCommand::parse();
    let sum = match command {
        AocCommand::Part1(io) => process_input(&io, part1)?,
        AocCommand::Part2(io) => process_input(&io, part2)?,
    };

    println!("Sum = {sum}");
    Ok(())
}

fn process_input<F>(io: &IOArgs, processor: F) -> Result<u32>
where
    F: FnOnce(&str) -> u32,
{
    let content = read_file(io.input_path.clone())?;
    Ok(processor(&content))
}

fn part1(input: &str) -> u32 {
    input
        .lines()
        .filter_map(|line| {
            let mut nums = line.chars().filter_map(|c| c.to_digit(10)).peekable();
            let first = *nums.peek()?;
            let last = nums.last()?;
            Some(first * 10 + last)
        })
        .sum()
}

fn part2(input: &str) -> u32 {
    input
        .lines()
        .filter_map(|mut line| {
            let mut first = None;
            let mut last = None;

            while !line.is_empty() {
                let (val, rest) = parse_digit(line);
                line = rest;
                if first.is_none() {
                    first = val;
                }
                if val.is_some() {
                    last = val;
                }
            }

            Some(first? * 10 + last?)
        })
        .sum()
}

// just an excuse to try out macros
macro_rules! parse_digit {
    ($s:expr, $pat:literal, $result:literal) => {
        if let Some(_) = $s.strip_prefix($pat) {
            return (Some($result), &$s[1..]);
        }
    };
}

fn parse_digit(s: &str) -> (Option<u32>, &str) {
    parse_digit!(s, "one", 1);
    parse_digit!(s, "1", 1);
    parse_digit!(s, "two", 2);
    parse_digit!(s, "2", 2);
    parse_digit!(s, "three", 3);
    parse_digit!(s, "3", 3);
    parse_digit!(s, "four", 4);
    parse_digit!(s, "4", 4);
    parse_digit!(s, "five", 5);
    parse_digit!(s, "5", 5);
    parse_digit!(s, "six", 6);
    parse_digit!(s, "6", 6);
    parse_digit!(s, "seven", 7);
    parse_digit!(s, "7", 7);
    parse_digit!(s, "eight", 8);
    parse_digit!(s, "8", 8);
    parse_digit!(s, "nine", 9);
    parse_digit!(s, "9", 9);

    (None, &s[1..])
}
