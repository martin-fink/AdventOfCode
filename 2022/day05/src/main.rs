fn main() {
    let lines: Vec<String> = common::read_lines("data/input.txt").expect("File could not be read");

    let inputs = lines.split(|x| x.is_empty()).collect::<Vec<_>>();
    let stack_inputs = inputs[0];
    let move_inputs = inputs[1];

    let n_stacks = (stack_inputs[0].len() + 1) / 4;
    let mut stacks: Vec<Vec<char>> = Vec::with_capacity(n_stacks);

    for _ in 0..n_stacks {
        stacks.push(Vec::new());
    }

    stack_inputs.iter().for_each(|line| {
        line.chars()
            .collect::<Vec<char>>()
            .chunks(4)
            // .map(|chunks| chunks.iter().collect::<String>())
            .enumerate()
            .for_each(|(index, chunk)| {
                if chunk[0] == '[' && chunk[2] == ']' {
                    stacks[index].push(chunk[1]);
                }
            });
    });

    for stack in stacks.iter_mut() {
        stack.reverse();
    }

    move_inputs
        .iter()
        .map(|line| {
            line.split(' ')
                .map(|word| word.parse::<usize>())
                .filter_map(|num| num.ok())
                .collect::<Vec<_>>()
        })
        .map(|nums| (nums[0], nums[1] - 1, nums[2] - 1))
        .for_each(|(n, from, to)| {
            let from_len = stacks[from].len();
            let mut elems = stacks[from].split_off(from_len - n);
            stacks[to].append(&mut elems);
            // for _ in 0..n {
            //     if let Some(elem) = stacks[from].pop() {
            //         stacks[to].push(elem);
            //     }
            // }
        });

    for (i, stack) in stacks.iter().enumerate() {
        println!("stack {i}: {:?}", stack);
    }

    for stack in stacks.iter() {
        if let Some(c) = stack.last() {
            print!("{c}");
        }
    }
    println!()
}
