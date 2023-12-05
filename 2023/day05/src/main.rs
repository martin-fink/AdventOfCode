use anyhow::Context;
use aoc::aoc_main;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;
use regex::Regex;
use std::collections::BTreeMap;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

#[derive(Default, Debug)]
struct SeedToLocationMap {
    seed_to_soil: BTreeMap<u64, (u64, u64)>,
    soil_to_fertilizer: BTreeMap<u64, (u64, u64)>,
    fertilizer_to_water: BTreeMap<u64, (u64, u64)>,
    water_to_light: BTreeMap<u64, (u64, u64)>,
    light_to_temp: BTreeMap<u64, (u64, u64)>,
    temp_to_humidity: BTreeMap<u64, (u64, u64)>,
    humidity_to_location: BTreeMap<u64, (u64, u64)>,
}

impl SeedToLocationMap {
    fn get(&self, seed: u64) -> u64 {
        let soil = Self::get_for_or_self(&self.seed_to_soil, seed);
        let fertilizer = Self::get_for_or_self(&self.soil_to_fertilizer, soil);
        let water = Self::get_for_or_self(&self.fertilizer_to_water, fertilizer);
        let light = Self::get_for_or_self(&self.water_to_light, water);
        let temp = Self::get_for_or_self(&self.light_to_temp, light);
        let humidity = Self::get_for_or_self(&self.temp_to_humidity, temp);

        Self::get_for_or_self(&self.humidity_to_location, humidity)
    }

    fn get_for_or_self(map: &BTreeMap<u64, (u64, u64)>, index: u64) -> u64 {
        Self::get_for(map, index).unwrap_or(index)
    }

    fn get_for(map: &BTreeMap<u64, (u64, u64)>, index: u64) -> Option<u64> {
        if let Some((to, range)) = map.get(&index) {
            assert_ne!(*range, 0);
            return Some(*to);
        }

        let (from, (to, range)) = map.range(..index).next_back()?;

        if index < from + range {
            let diff = index - from;
            Some(to + diff)
        } else {
            None
        }
    }
}

fn parse_input(s: &str) -> anyhow::Result<(Vec<u64>, SeedToLocationMap)> {
    let regex = Regex::new("seeds: ([\\d ]+)\\s+seed-to-soil map:([\\s\\d]+)\\nsoil-to-fertilizer map:([\\s\\d]+)\\nfertilizer-to-water map:([\\s\\d]+)\\nwater-to-light map:([\\s\\d]+)\\nlight-to-temperature map:([\\s\\d]+)\\ntemperature-to-humidity map:([\\s\\d]+)\\nhumidity-to-location map:([\\s\\d]+)\\n")?;

    let matches = regex.captures(s).context("unable to match regex")?;

    let map = SeedToLocationMap {
        seed_to_soil: parse_map(matches.get(2).context("unable to get capture")?.as_str()),
        soil_to_fertilizer: parse_map(matches.get(3).context("unable to get capture")?.as_str()),
        fertilizer_to_water: parse_map(matches.get(4).context("unable to get capture")?.as_str()),
        water_to_light: parse_map(matches.get(5).context("unable to get capture")?.as_str()),
        light_to_temp: parse_map(matches.get(6).context("unable to get capture")?.as_str()),
        temp_to_humidity: parse_map(matches.get(7).context("unable to get capture")?.as_str()),
        humidity_to_location: parse_map(matches.get(8).context("unable to get capture")?.as_str()),
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
        .map(|seed| map.get(*seed))
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

        for i in 0..len {
            all_seeds.push(start + i);
        }
    }

    let all = all_seeds.len();
    eprintln!("looking for {all} seeds");

    let min = all_seeds
        .par_iter()
        .map(|seed| map.get(*seed))
        .min()
        .context("unable to find min")?;

    eprintln!();

    Ok(min)
}
