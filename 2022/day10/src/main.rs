use std::str::FromStr;

enum Instruction {
    AddX(i32),
    Noop,
}

impl Instruction {
    fn get_cycles(&self) -> usize {
        match self {
            Instruction::AddX(_) => 2,
            Instruction::Noop => 1,
        }
    }
}

#[derive(Debug)]
struct ParseError {}

impl From<std::num::ParseIntError> for ParseError {
    fn from(_: std::num::ParseIntError) -> Self {
        ParseError {}
    }
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "noop" {
            Ok(Instruction::Noop)
        } else if s.starts_with("addx") {
            let split = s.split(' ').collect::<Vec<_>>();
            let i = split[1].parse::<i32>()?;

            Ok(Instruction::AddX(i))
        } else {
            Err(ParseError {})
        }
    }
}

struct Cpu<F>
where
    F: FnMut(usize, i32),
{
    x: i32,
    clock: usize,
    hook: F,
}

impl<F> Cpu<F>
where
    F: FnMut(usize, i32),
{
    fn new(f: F) -> Self {
        Cpu {
            x: 1,
            clock: 0,
            hook: f,
        }
    }

    fn execute_instr(&mut self, instr: &Instruction) {
        for _ in 0..instr.get_cycles() {
            self.clock += 1;
            (self.hook)(self.clock, self.x);
        }
        match instr {
            Instruction::AddX(i) => self.x += i,
            Instruction::Noop => {}
        }
    }
}

fn main() {
    let input = common::read_lines("data/input.txt").expect("File could not be read");

    let instructions = input
        .iter()
        .map(|line| Instruction::from_str(line).unwrap())
        .collect::<Vec<_>>();

    let mut signal_strength = 0;
    let mut cpu = Cpu::new(|cycle, x| {
        if cycle == 20
            || cycle == 60
            || cycle == 100
            || cycle == 140
            || cycle == 180
            || cycle == 220
        {
            signal_strength += cycle as isize * x as isize;
        }
    });

    for instruction in instructions.iter() {
        cpu.execute_instr(instruction);
    }

    println!("{signal_strength}");

    let mut cpu = Cpu::new(|cycle, x| {
        let curr_pos = (cycle as isize - 1) % 40;

        if (x as isize - curr_pos as isize).abs() <= 1 {
            print!("██");
        } else {
            print!("░░");
        }

        if curr_pos == 39 {
            println!()
        }
    });

    for instruction in instructions.iter() {
        cpu.execute_instr(instruction);
    }

    // println!("{signal_strength}");
}
