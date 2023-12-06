use aoc::aoc_main;
use regex::Regex;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

fn get_distance(time_charging: u64, time_remaining: u64) -> u64 {
    time_charging * time_remaining
}

fn parse_numbers(s: &str) -> Vec<u64> {
    let regex = Regex::new("(\\d+)").unwrap();

    regex
        .captures_iter(s)
        .map(|n| n.get(0).unwrap().as_str().parse::<u64>().unwrap())
        .collect()
}

fn get_wins(time: u64, record: u64) -> u64 {
    let mut wins = 0;
    for time_charging in 0..=time {
        if get_distance(time_charging, time - time_charging) > record {
            wins += 1;
        } else if wins > 0 {
            // now we're not finding any new wins
            break;
        }
    }

    wins
}

fn part1(s: &str) -> anyhow::Result<u64> {
    let (times, distances) = parse_input(s)?;

    let result = times
        .iter()
        .zip(distances.iter())
        .map(|(time, distance)| get_wins(*time, *distance))
        .product();

    Ok(result)
}

fn parse_input(s: &str) -> anyhow::Result<(Vec<u64>, Vec<u64>)> {
    let regex = Regex::new("^.*:\\s+(.*)")?;

    let mut values = s
        .lines()
        .map(|line| regex.captures(line).unwrap().get(1).unwrap().as_str())
        .map(parse_numbers)
        .collect::<Vec<_>>();

    assert_eq!(values.len(), 2);

    let distances = values.remove(1);
    let times = values.remove(0);
    Ok((times, distances))
}

fn concat_nums(mut a: u64, b: &u64) -> u64 {
    let mut copy = *b;
    while copy > 0 {
        copy /= 10;
        a *= 10;
    }

    a + *b
}

fn part2(s: &str) -> anyhow::Result<u64> {
    let (times, distances) = parse_input(s)?;

    let time = times.iter().fold(0u64, concat_nums);
    let distance = distances.iter().fold(0u64, concat_nums);

    let result = get_wins(time, distance);

    Ok(result)
}

#[cfg(test)]
mod tests {
    use crate::concat_nums;

    #[test]
    fn test_concat() {
        let a = 10;
        let b = 5;
        assert_eq!(concat_nums(a, &b), 105);
    }
}
