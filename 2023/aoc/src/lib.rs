use std::fs::File;
use std::io::Read;
use anyhow::Context;
use clap::Parser;

#[derive(clap::Parser, Debug)]
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

fn read_file(path: std::path::PathBuf) -> anyhow::Result<String> {
    let file = File::open(&path).with_context(|| format!("Failed to open {}", path.display()))?;
    let mut buf_reader = std::io::BufReader::new(file);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content)?;
    Ok(content)
}

pub fn aoc_main<F1, F2, R>(part1: F1, part2: F2) -> anyhow::Result<R> where F1: FnOnce(&str) -> R, F2: FnOnce(&str) -> R {
    let command = AocCommand::parse();
    let result = match command {
        AocCommand::Part1(io) => process_input(&io, part1)?,
        AocCommand::Part2(io) => process_input(&io, part2)?,
    };

    Ok(result)
}

fn process_input<F, R>(io: &IOArgs, processor: F) -> anyhow::Result<R>
    where
        F: FnOnce(&str) -> R,
{
    let content = read_file(io.input_path.clone())?;
    Ok(processor(&content))
}
