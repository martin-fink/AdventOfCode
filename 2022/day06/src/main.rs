use std::collections::HashSet;

fn main() {
    let input = common::read_file("data/input.txt").expect("File could not be read");

    let vec = input.chars().collect::<Vec<_>>();
    let slice = &vec[..];

    let res = slice
        .windows(4)
        .enumerate()
        .find(|(_, window)| {
            window[0] != window[1]
                && window[0] != window[2]
                && window[0] != window[3]
                && window[1] != window[2]
                && window[1] != window[3]
                && window[2] != window[3]
        })
        .map(|(i, _)| i + 4)
        .unwrap();

    println!("{res}");

    let res = slice
        .windows(14)
        .enumerate()
        .find(|(_, window)| {
            let mut set = HashSet::with_capacity(14);

            window.iter().for_each(|c| {
                set.insert(c);
            });

            set.len() == 14
        })
        .map(|(i, _)| i + 14)
        .unwrap();

    println!("{res}");
}
