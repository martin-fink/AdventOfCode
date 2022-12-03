use std::collections::HashSet;

fn main() {
    let input = common::read_file("data/input.txt").expect("File could not be read");

    let lines: Vec<&str> = input.split('\n').collect();

    let result: u32 = lines
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| (&line[0..line.len() / 2], &line[line.len() / 2..line.len()]))
        .map(find_duplicate)
        .map(find_value)
        .sum();

    println!("{result}");

    let result2 = lines
        .iter()
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .chunks(3)
        .map(find_duplicate_triple)
        .map(find_value)
        .sum::<u32>();

    println!("{result2}");
}

fn find_duplicate_triple(triple: &[&&str]) -> char {
    let mut set0 = HashSet::new();
    let mut set1 = HashSet::new();

    triple[0].chars().for_each(|c| {
        set0.insert(c);
    });
    triple[1].chars().for_each(|c| {
        set1.insert(c);
    });

    for c in triple[2].chars() {
        if set0.contains(&c) && set1.contains(&c) {
            return c;
        }
    }

    unreachable!()
}

fn find_duplicate(tuple: (&str, &str)) -> char {
    let mut set = HashSet::new();

    tuple.0.chars().for_each(|c| {
        set.insert(c);
    });

    for c in tuple.1.chars() {
        if set.contains(&c) {
            return c;
        }
    }

    println!("{}, {}", tuple.0, tuple.1);

    unreachable!()
}

fn find_value(c: char) -> u32 {
    let c_val: u32 = c.into();
    let (x, add) = match c {
        'a'..='z' => ('a', 1),
        'A'..='Z' => ('A', 27),
        _ => unreachable!(),
    };

    let x_val: u32 = x.into();

    c_val - x_val + add
}
