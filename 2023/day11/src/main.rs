use anyhow::Result;
use aoc::aoc_main;

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}

type Pos = (usize, usize);

fn get_galaxies(s: &str, multiplier: usize) -> Vec<Pos> {
    let mut curr_line = 0;
    let width = s.find('\n').unwrap();
    let lines = s.len() / width;
    let mut galaxies = Vec::new();

    let mut col_expansions = vec![0; width];

    for c in 0..width {
        let mut has_galaxy = false;
        for l in 0..lines {
            if s.as_bytes()[l * (width + 1) + c] as char == '#' {
                has_galaxy = true;
                break;
            }
        }
        col_expansions[c] =
            col_expansions[c.saturating_sub(1)] + if has_galaxy { 1 } else { multiplier }
    }

    for l in 0..lines {
        let mut found_galaxy = false;
        for (c, curr_col) in col_expansions.iter().enumerate() {
            if s.as_bytes()[l * (width + 1) + c] as char == '#' {
                galaxies.push((curr_line, *curr_col));
                found_galaxy = true;
            }
        }
        curr_line += if found_galaxy { 1 } else { multiplier }
    }

    galaxies
}

fn count_distances(s: &str, expansion: usize) -> usize {
    let galaxies = get_galaxies(s, expansion);
    let mut distance = 0;

    for i in 0..galaxies.len() {
        for j in i + 1..galaxies.len() {
            let (x1, y1) = galaxies[i];
            let (x2, y2) = galaxies[j];
            distance += x2.abs_diff(x1) + y2.abs_diff(y1);
        }
    }

    distance
}

fn part1(s: &str) -> usize {
    count_distances(s, 2)
}

fn part2(s: &str) -> usize {
    count_distances(s, 1000000)
}
