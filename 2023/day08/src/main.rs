use std::fs::File;
use std::hint::unreachable_unchecked;
use std::io::Read;
use std::mem::MaybeUninit;

fn main() {
    let path = std::env::args().nth(2).unwrap();

    let file = File::open(path).unwrap();
    let mut buf_reader = std::io::BufReader::new(file);
    let mut content = String::new();
    buf_reader.read_to_string(&mut content).unwrap();

    let result = if std::env::args().nth(1).unwrap() == "part1" {
        part1(&content)
    } else {
        part2(&content)
    };

    println!("{result}");
}

// 26^3 = 17576
type PathMap<'a> = [MaybeUninit<(&'a str, &'a str)>; 32 * 32 * 32];

#[inline]
fn hash_letters_str(letters: &str) -> usize {
    debug_assert!(letters.len() == 3);
    let bytes = letters.as_bytes();
    (((bytes[0] - 0x41) as usize) * 32 * 32)
        | (((bytes[1] - 0x41) as usize) * 32)
        | ((bytes[2] - 0x41) as usize)
}

#[inline]
fn parse_line(s: &str) -> (&str, (&str, &str)) {
    (&s[0..3], (&s[7..10], &s[12..15]))
}

fn shortest_path(directions: &str, maps: &PathMap, start: &str) -> usize {
    let mut steps = 0usize;
    let mut current = start;

    while current.as_bytes()[2] as char != 'Z' {
        let instruction = unsafe { maps[hash_letters_str(current)].assume_init() };
        current = match directions.as_bytes()[steps % directions.len()] as char {
            'L' => instruction.0,
            'R' => instruction.1,
            _ => unsafe { unreachable_unchecked() },
        };

        steps += 1;
    }

    steps
}

fn parse_input(s: &str) -> (&str, Vec<&str>, Box<PathMap>) {
    let mut lines = s.lines();

    let directions = lines.next().unwrap();

    let mut beginnings = Vec::new();
    let mut maps = Box::new([MaybeUninit::uninit(); 32 * 32 * 32]);

    lines.skip(1).map(parse_line).for_each(|(k, v)| {
        maps[hash_letters_str(k)] = MaybeUninit::new(v);
        if k.as_bytes()[2] as char == 'A' {
            beginnings.push(k);
        }
    });

    (directions, beginnings, maps)
}

#[inline]
fn part1(s: &str) -> u32 {
    let (directions, _, maps) = parse_input(s);

    shortest_path(directions, &maps, "AAA") as u32
}

#[inline]
fn part2(s: &str) -> u32 {
    let (directions, beginnings, maps) = parse_input(s);

    let steps = beginnings
        .iter()
        .map(|k| shortest_path(directions, &maps, k) as u32)
        .fold(1, lcm);

    steps
}

#[inline]
fn lcm(a: u32, other: u32) -> u32 {
    if a == 0 && other == 0 {
        return 0;
    }
    let gcd = gcd(a, other);
    
    a * (other / gcd)
}

#[inline]
fn gcd(a: u32, other: u32) -> u32 {
    // Use Stein's algorithm
    let mut m = a;
    let mut n = other;
    if m == 0 || n == 0 {
        return m | n;
    }

    // find common factors of 2
    let shift = (m | n).trailing_zeros();

    // divide n and m by 2 until odd
    m >>= m.trailing_zeros();
    n >>= n.trailing_zeros();

    while m != n {
        if m > n {
            m -= n;
            m >>= m.trailing_zeros();
        } else {
            n -= m;
            n >>= n.trailing_zeros();
        }
    }
    m << shift
}
