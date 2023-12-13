use anyhow::Result;
use aoc::aoc_main;
use std::collections::HashMap;
use std::iter;

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}

fn parse_line(line: &str) -> (&str, Vec<usize>) {
    let mut splits = line.split(' ');
    let str = splits.next().unwrap();
    let nums = splits
        .next()
        .unwrap()
        .split(',')
        .map(|n| n.parse().unwrap())
        .collect();

    (str, nums)
}

fn matches<'a>(
    mut s: &'a str,
    lengths: &'a [usize],
    cache: &mut HashMap<(&'a str, &'a [usize]), usize>,
) -> usize {
    // first skip dots
    while !s.is_empty() && s.as_bytes()[0] as char == '.' {
        s = &s[1..];
    }

    if let Some(m) = cache.get(&(s, lengths)) {
        return *m;
    }

    if lengths.is_empty() {
        return if s.bytes().any(|c| c as char == '#') {
            0
        } else {
            1
        };
    }
    let curr_len = lengths[0];
    assert_ne!(curr_len, 0);
    if s.len() < curr_len {
        return 0;
    }

    // now s[0] is either ? or #
    let current = s.as_bytes()[0] as char;

    let is_ok = s.bytes().take(curr_len).all(|b| b as char != '.')
        && (s.len() == curr_len || s.as_bytes()[curr_len] as char != '#');
    let result = if current == '#' {
        if is_ok {
            matches(
                &s[usize::min(curr_len + 1, s.len())..],
                &lengths[1..],
                cache,
            )
        } else {
            0
        }
    } else if current == '?' {
        matches(&s[1..], lengths, cache)
            + if is_ok {
                matches(
                    &s[usize::min(curr_len + 1, s.len())..],
                    &lengths[1..],
                    cache,
                )
            } else {
                0
            }
    } else {
        unreachable!()
    };

    cache.insert((s, lengths), result);
    result
}

fn part1(s: &str) -> usize {
    s.lines()
        .map(parse_line)
        .map(|(s, lengths)| {
            let mut cache = HashMap::new();
            let matches = matches(s, &lengths, &mut cache);
            println!("{s} {lengths:?}: {matches}");
            matches
        })
        .sum()
}

fn part2(s: &str) -> usize {
    s.lines()
        .map(parse_line)
        .map(|(s, lengths)| {
            let s = iter::repeat(s).take(5).collect::<Vec<_>>().join("?");
            let lengths = iter::repeat(lengths)
                .take(5)
                .flatten()
                .collect::<Vec<_>>();
            (s, lengths)
        })
        .map(|(s, lengths)| {
            let mut cache = HashMap::new();
            let matches = matches(&s, &lengths, &mut cache);
            println!("{s} {lengths:?}: {matches}");
            matches
        })
        .sum()
}

#[cfg(test)]
mod tests {
    use crate::matches;

    #[test]
    fn test1() {
        assert_eq!(matches("???", &[1], &mut Default::default()), 3);
    }

    #[test]
    fn test2() {
        assert_eq!(matches("????", &[2, 1], &mut Default::default()), 1);
    }

    #[test]
    fn test3() {
        assert_eq!(matches("???????????", &[2, 5], &mut Default::default()), 10);
    }

    #[test]
    fn test4() {
        assert_eq!(matches(".#??#", &[2], &mut Default::default()), 0);
    }
}
