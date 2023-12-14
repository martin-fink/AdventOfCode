use anyhow::Result;
use aoc::aoc_main;

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}

fn find_vertical_reflection(s: &str, smudges: usize) -> usize {
    let width = s.find('\n').unwrap() + 1;
    let height = (s.len() + 1) / width;

    let cols_eq = |c1, c2| {
        let bytes = s.as_bytes();
        let mut diff = 0;
        for i in 0..height {
            if bytes[i * width + c1] != bytes[i * width + c2] {
                diff += 1;
            }
        }

        diff
    };

    for c in 1..width - 1 {
        let mut diff = 0;
        for i in 0..usize::min(c, width - 1 - c) {
            diff += cols_eq(c - i - 1, c + i);
        }
        if diff == smudges {
            return c;
        }
    }

    0
}

fn find_horizontal_reflection(s: &str, smudges: usize) -> usize {
    let width = s.find('\n').unwrap() + 1;
    let height = (s.len() + 1) / width;

    let rows_eq = |r1, r2| {
        let bytes = s.as_bytes();
        let mut diff = 0;
        for i in 0..width - 1 {
            if bytes[r1 * width + i] != bytes[r2 * width + i] {
                diff += 1;
            }
        }

        diff
    };

    for r in 1..height {
        let mut diff = 0;
        for i in 0..usize::min(r, height - r) {
            diff += rows_eq(r - i - 1, r + i);
        }
        if diff == smudges {
            return r;
        }
    }

    0
}

fn find_reflections(s: &str, smudges: usize) -> usize {
    let v = find_vertical_reflection(s, smudges);
    let h = find_horizontal_reflection(s, smudges);
    assert!(v == 0 || h == 0);

    v + h * 100
}

fn part1(s: &str) -> usize {
    s.split("\n\n").map(|s| find_reflections(s, 0)).sum()
}

fn part2(s: &str) -> usize {
    s.split("\n\n").map(|s| find_reflections(s, 1)).sum()
}
