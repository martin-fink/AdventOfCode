use anyhow::Context;
use aoc::aoc_main;
use regex::Regex;
use std::collections::BTreeMap;
use std::ops::Range;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

#[derive(Default, Debug)]
struct SeedToLocationMap {
    maps: Vec<BTreeMap<u64, (u64, u64)>>,
}

impl SeedToLocationMap {
    fn get(&self, val: Range<u64>) -> u64 {
        let mut worklist = vec![val];
        for map in &self.maps {
            let mut tmp = Vec::new();
            for item in worklist {
                tmp.extend(Self::get_for_range(map, item));
            }
            worklist = tmp;
        }

        worklist.iter().map(|r| r.start).min().unwrap()
    }

    fn get_for_range(map: &BTreeMap<u64, (u64, u64)>, mut index: Range<u64>) -> Vec<Range<u64>> {
        let mut worklist = Vec::new();

        // first check whatever is overhanging to the left
        if let Some((lower, (to, len))) = map.range(..index.start).next_back()
        {
            let n_overlapping = u64::min(
                index.end - index.start,
                lower.saturating_add(*len).saturating_sub(index.start),
            );
            assert!(*len >= n_overlapping);
            let diff = index.start - *lower;

            let new_range = to + diff..to + diff + n_overlapping;

            if !new_range.is_empty() {
                worklist.push(new_range);
            }
            index.start += n_overlapping;
        }

        let range = map.range(index.clone());
        for (upper, (to, len)) in range {
            if *upper != index.start {
                assert!(*upper > index.start);
                let n_not_overlapping = u64::min(*upper - index.start, index.end - index.start);
                worklist.push(index.start..index.start + n_not_overlapping);
                index.start = *upper;
            }
            let n_overlapping = u64::min(*len, index.end - index.start);
            worklist.push(*to..to + n_overlapping);
            index.start += n_overlapping;
        }
        if !index.is_empty() {
            worklist.push(index);
        }

        worklist
    }
}

fn parse_input(s: &str) -> anyhow::Result<(Vec<u64>, SeedToLocationMap)> {
    let regex = Regex::new("seeds: ([\\d ]+)\\s+seed-to-soil map:([\\s\\d]+)\\nsoil-to-fertilizer map:([\\s\\d]+)\\nfertilizer-to-water map:([\\s\\d]+)\\nwater-to-light map:([\\s\\d]+)\\nlight-to-temperature map:([\\s\\d]+)\\ntemperature-to-humidity map:([\\s\\d]+)\\nhumidity-to-location map:([\\s\\d]+)\\n")?;

    let matches = regex.captures(s).context("unable to match regex")?;

    let map = SeedToLocationMap {
        maps: matches
            .iter()
            .skip(2)
            .map(|m| parse_map(m.unwrap().as_str()))
            .collect(),
    };

    let seeds = matches
        .get(1)
        .unwrap()
        .as_str()
        .trim()
        .split(' ')
        .map(|s| s.parse::<u64>().unwrap())
        .collect();

    Ok((seeds, map))
}

fn part1(s: &str) -> anyhow::Result<u64> {
    let (seeds, map) = parse_input(s)?;

    let min = seeds
        .iter()
        .map(|seed| map.get(*seed..*seed + 1))
        .min()
        .context("unable to find min")?;

    Ok(min)
}

fn parse_map(s: &str) -> BTreeMap<u64, (u64, u64)> {
    s.trim()
        .lines()
        .map(|l| {
            let splits = l
                .trim()
                .split(' ')
                .map(|s| s.parse::<u64>().unwrap())
                .collect::<Vec<u64>>();

            assert_eq!(splits.len(), 3);

            (
                *splits.get(1).unwrap(),
                (*splits.first().unwrap(), *splits.get(2).unwrap()),
            )
        })
        .collect()
}

fn part2(s: &str) -> anyhow::Result<u64> {
    let (seeds, map) = parse_input(s)?;

    assert_eq!(seeds.len() % 2, 0);

    let mut all_seeds = Vec::new();

    for i in 0..(seeds.len() / 2) {
        let index = i * 2;
        let start = seeds[index];
        let len = seeds[index + 1];

        all_seeds.push(start..start + len);
    }

    let min = all_seeds
        .iter()
        .cloned()
        .map(|seed| map.get(seed))
        .min()
        .context("unable to find min")?;

    Ok(min)
}
