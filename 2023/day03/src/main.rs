use aoc::aoc_main;

fn main() -> anyhow::Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

fn part1(input: &str) -> anyhow::Result<u32> {
    let mut sum = 0;
    let width = input.lines().next().unwrap().len();
    let empty: String = ".".repeat(width);
    let empty_ref: &str = &empty;

    let up = std::iter::once(empty_ref).chain(input.lines());
    let mid = input.lines();
    let down = input.lines().chain(std::iter::once(empty_ref)).skip(1);

    itertools::izip!(up, mid, down).for_each(|(up, mid, down)| {
        let mut acc = 0;
        let mut is_part = false;

        let bytes_up = up.as_bytes();
        let bytes_down = down.as_bytes();
        let bytes = mid.as_bytes();
        for i in 0..mid.len() {
            let digit = bytes[i] as char;
            if let Some(digit) = digit.to_digit(10) {
                acc = acc * 10 + digit;
                is_part |= is_symbol_or_digit(bytes_up[i]) || is_symbol_or_digit(bytes_down[i]);
                if i > 0 {
                    is_part |= is_symbol(bytes[i - 1])
                        || is_symbol_or_digit(bytes_up[i - 1])
                        || is_symbol_or_digit(bytes_down[i - 1])
                }
                if i < mid.len() - 1 {
                    is_part |= is_symbol(bytes[i + 1])
                        || is_symbol_or_digit(bytes_up[i + 1])
                        || is_symbol_or_digit(bytes_down[i + 1])
                }
            } else {
                // let byte = bytes[i] as char;
                // if acc > 0 {
                //     if is_part {
                //         print!("{}", format!("{acc}").green());
                //     } else {
                //         print!("{}", format!("{acc}").red());
                //     }
                // }
                // if is_symbol(bytes[i]) {
                //     print!("{}", format!("{byte}").blue());
                // } else {
                //     print!(".")
                // }
                if is_part {
                    sum += acc;
                }
                acc = 0;
                is_part = false;
            }
        }
        if is_part {
            sum += acc;
        }

        // println!()
    });

    Ok(sum)
}

fn is_symbol(b: u8) -> bool {
    let c = b as char;
    c != '.' && !c.is_ascii_digit()
}

fn is_symbol_or_digit(b: u8) -> bool {
    b as char != '.'
}

fn get_num(s: &str, index: usize) -> Option<(u32, usize)> {
    if s.is_empty() || index >= s.len() {
        return None;
    }
    if !s.as_bytes()[index].is_ascii_digit() {
        return None;
    }

    // go as far left as possible
    let mut start = index;
    while start > 0 && (s.as_bytes()[start - 1] as char).is_ascii_digit() {
        start -= 1;
    }

    let mut end = index;
    // now go as far right as possible
    while end < s.len() && (s.as_bytes()[end] as char).is_ascii_digit() {
        end += 1;
    }

    s[start..end].parse().ok().map(|num| (num, end + 1))
}

fn part2(input: &str) -> anyhow::Result<u32> {
    let mut sum = 0;
    let width = input.lines().next().unwrap().len();
    let empty: String = ".".repeat(width);
    let empty_ref: &str = &empty;

    let up = std::iter::once(empty_ref).chain(input.lines());
    let mid = input.lines();
    let down = input.lines().chain(std::iter::once(empty_ref)).skip(1);

    itertools::izip!(up, mid, down).for_each(|(up, mid, down)| {
        let bytes = mid.as_bytes();

        for (i, c) in bytes.iter().enumerate() {
            let mut nums = Vec::new();
            if (*c as char) == '*' {
                for (s, index) in [(mid, i.saturating_sub(1)), (mid, i.saturating_add(1))] {
                    if let Some((num, _)) = get_num(s, index) {
                        nums.push(num);
                    }
                }

                for s in [up, down] {
                    let mut j = i.saturating_sub(1);

                    while j < s.len() && j <= i.saturating_add(1) {
                        if let Some((num, next)) = get_num(s, j) {
                            nums.push(num);
                            j = next;
                        } else {
                            j += 1;
                        }
                    }
                }

                if nums.len() > 1 {
                    assert_eq!(nums.len(), 2);
                    // eprintln!("multiplying {:?}", nums);
                    sum += nums.iter().product::<u32>();
                }
            }
        }
    });

    Ok(sum)
}
