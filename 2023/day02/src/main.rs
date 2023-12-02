use aoc::aoc_main;
use regex::Regex;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

#[derive(Default)]
struct Colors {
    r: u32,
    g: u32,
    b: u32,
}

fn part1(input: &str) -> anyhow::Result<u32> {
    let game_idregex = Regex::new("^Game (\\d+):")?;

    let sum = input
        .lines()
        .flat_map(|line| {
            let id = game_idregex
                .captures(line)
                .unwrap()
                .get(1)
                .unwrap()
                .as_str()
                .parse::<u32>()
                .unwrap();
            let impossible = line
                .split(';')
                .flat_map(count_colors)
                .any(|colors| colors.r > 12 || colors.g > 13 || colors.b > 14);
            if impossible {
                None
            } else {
                Some(id)
            }
        })
        .sum();
    Ok(sum)
}

fn count_colors(s: &str) -> anyhow::Result<Colors> {
    let mut colors = Colors::default();
    let regex = Regex::new("(\\d+) (red|green|blue)")?;
    for capture in regex.captures_iter(s) {
        let num = capture.get(1).unwrap().as_str().parse::<u32>()?;
        let color = capture.get(2).unwrap().as_str();
        match color {
            "red" => colors.r = u32::max(colors.r, num),
            "green" => colors.g = u32::max(colors.g, num),
            "blue" => colors.b = u32::max(colors.b, num),
            _ => panic!(),
        }
    }

    Ok(colors)
}

fn part2(input: &str) -> anyhow::Result<u32> {
    let sum = input
        .lines()
        .flat_map(|line| {
            let colors = count_colors(line).ok()?;
            Some(colors.b * colors.r * colors.g)
        })
        .sum();
    Ok(sum)
}
