use aoc::aoc_main;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)?;

    println!("{result}");

    Ok(())
}

fn hash(hash: u8, byte: u8) -> u8 {
    let mut hash = hash as u16;

    hash += byte as u16;
    hash *= 17;

    hash as u8
}

fn hash_str(s: &str) -> u8 {
    s.bytes().fold(0, hash)
}

fn part1(s: &str) -> u32 {
    s.split(',').map(hash_str).map(u32::from).sum()
}

struct Item<'a> {
    s: &'a str,
    focal_len: u32,
}

struct HolidayMap<'a> {
    buckets: [Vec<Item<'a>>; 256],
}

impl<'a> HolidayMap<'a> {
    fn new() -> Self {
        const ITEM: Vec<Item> = Vec::new();
        Self {
            buckets: [ITEM; 256],
        }
    }

    fn put(&mut self, s: &'a str, focal_len: u32) {
        let bucket = hash_str(s);
        let item = self.buckets[bucket as usize]
            .iter_mut()
            .find(|item| item.s == s);
        if let Some(item) = item {
            item.focal_len = focal_len;
        } else {
            self.buckets[bucket as usize].push(Item { s, focal_len })
        }
    }

    fn remove(&mut self, s: &'a str) {
        let bucket = hash_str(s);
        let bucket = &mut self.buckets[bucket as usize];
        for i in 0..bucket.len() {
            if bucket[i].s == s {
                bucket.remove(i);
                return;
            }
        }
    }
}

enum HashMapOperation {
    Set(u32),
    Remove,
}

fn parse_op(s: &str) -> (&str, HashMapOperation) {
    if s.as_bytes()[s.len() - 1] as char == '-' {
        (&s[..s.len() - 1], HashMapOperation::Remove)
    } else {
        let idx = s.rfind('=').unwrap();
        let num = s[idx + 1..].parse().unwrap();
        let s = &s[..idx];
        (s, HashMapOperation::Set(num))
    }
}

fn part2(s: &str) -> u32 {
    let mut map = HolidayMap::new();
    for (s, op) in s.split(',').map(parse_op) {
        match op {
            HashMapOperation::Set(n) => {
                map.put(s, n);
            }
            HashMapOperation::Remove => {
                map.remove(s);
            }
        }
    }

    map.buckets
        .iter()
        .enumerate()
        .map(|(index, bucket)| -> u32 {
            (index + 1) as u32
                * bucket
                    .iter()
                    .enumerate()
                    .map(|(index, bucket)| (index + 1) as u32 * bucket.focal_len)
                    .sum::<u32>()
        })
        .sum()
}

#[cfg(test)]
mod test {
    use crate::hash_str;

    #[test]
    fn test1() {
        let str = "HASH";
        assert_eq!(hash_str(str), 52);
    }
}
