use std::{str::FromStr, vec};

use regex::Regex;

enum ExpressionVar {
    Old,
    Constant(u64),
}

impl FromStr for ExpressionVar {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "old" {
            Ok(Self::Old)
        } else {
            Ok(Self::Constant(s.parse::<u64>().unwrap()))
        }
    }
}

impl ExpressionVar {
    fn get(&self) -> Option<u64> {
        match self {
            ExpressionVar::Old => None,
            ExpressionVar::Constant(c) => Some(*c),
        }
    }
}

impl std::fmt::Display for ExpressionVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionVar::Old => write!(f, "old"),
            ExpressionVar::Constant(c) => write!(f, "{}", c),
        }
    }
}

enum ExpressionOperation {
    Mul,
    Add,
}

impl ExpressionOperation {
    fn exec(&self, lhs: u64, rhs: u64) -> u64 {
        match self {
            ExpressionOperation::Mul => lhs * rhs,
            ExpressionOperation::Add => lhs + rhs,
        }
    }
}

impl FromStr for ExpressionOperation {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "*" => Ok(Self::Mul),
            "+" => Ok(Self::Add),
            _ => panic!("Not implemented"),
        }
    }
}

impl std::fmt::Display for ExpressionOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExpressionOperation::Mul => write!(f, "*"),
            ExpressionOperation::Add => write!(f, "+"),
        }
    }
}

struct Operation {
    lhs: ExpressionVar,
    rhs: ExpressionVar,
    op: ExpressionOperation,
}

impl Operation {
    fn exec(&self, old: u64) -> u64 {
        self.op
            .exec(self.lhs.get().unwrap_or(old), self.rhs.get().unwrap_or(old))
    }
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} {}", self.lhs, self.op, self.rhs)
    }
}

struct Monkey {
    items: Vec<u64>,
    operation: Operation,
    test_divisible: u64,
    throw_false: usize,
    throw_true: usize,
    comparisons: usize,
}

impl std::fmt::Display for Monkey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Monkey:\n  items: {:?}\n  op: {}\n  test: {}\n  false: {}\n  true: {}",
            self.items, self.operation, self.test_divisible, self.throw_false, self.throw_true
        )
    }
}

const REGEX_STR: &str = "Monkey \\d:\\s+Starting items: ((?:\\d+(?:, )?)+)\\s+Operation: new = (old|\\d+) (\\*|\\+) (old|\\d+)\\s+Test: divisible by (\\d+)\\s+If true: throw to monkey (\\d+)\\s+If false: throw to monkey (\\d+)";

fn main() {
    let input = common::read_file("data/input.txt").expect("File could not be read");

    let r = Regex::new(REGEX_STR).unwrap();

    let mut monkeys = r
        .captures_iter(&input)
        .map(|cap| {
            let items = cap[1]
                .split(", ")
                .map(|i| i.parse::<u64>().unwrap())
                .collect::<Vec<_>>();
            Monkey {
                items,
                operation: Operation {
                    lhs: ExpressionVar::from_str(&cap[2]).unwrap(),
                    rhs: ExpressionVar::from_str(&cap[4]).unwrap(),
                    op: ExpressionOperation::from_str(&cap[3]).unwrap(),
                },
                test_divisible: cap[5].parse().unwrap(),
                throw_false: cap[7].parse().unwrap(),
                throw_true: cap[6].parse().unwrap(),
                comparisons: 0,
            }
        })
        .collect::<Vec<_>>();

    let lcm = monkeys
        .iter()
        .map(|m| m.test_divisible)
        .reduce(num::integer::lcm)
        .unwrap();

    println!("{}", lcm);

    for _ in 0..10000 {
        for i in 0..monkeys.len() {
            let mut workset: Vec<u64> = vec![];
            std::mem::swap(&mut workset, &mut monkeys[i].items);
            monkeys[i].comparisons += workset.len();

            for worry_level in workset {
                let monkey = &monkeys[i];
                let new = monkey.operation.exec(worry_level) % lcm;

                let monkey_index = if new % monkey.test_divisible == 0 {
                    monkey.throw_true
                } else {
                    monkey.throw_false
                };

                monkeys[monkey_index].items.push(new);
            }
        }
    }

    let mut max1 = 0;
    let mut max2 = 0;

    monkeys.iter().for_each(|m| {
        if m.comparisons > max1 {
            max2 = max1;
            max1 = m.comparisons;
        } else if m.comparisons > max2 {
            max2 = m.comparisons;
        }
    });

    println!("{max1} * {max2} = {}", max1 * max2);
}
