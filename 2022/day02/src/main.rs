use std::str::FromStr;

#[derive(PartialEq, Eq, Clone, Copy)]
enum RockPaperScissors {
    Rock,
    Paper,
    Scissors,
}

#[derive(Debug)]
struct ParseError(String);

impl FromStr for RockPaperScissors {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(RockPaperScissors::Rock),
            "B" => Ok(RockPaperScissors::Paper),
            "C" => Ok(RockPaperScissors::Scissors),
            _ => Err(ParseError("Could not parse rock paper scissors".into())),
        }
    }
}

impl RockPaperScissors {
    fn get_points(&self) -> u32 {
        match self {
            RockPaperScissors::Rock => 1,
            RockPaperScissors::Paper => 2,
            RockPaperScissors::Scissors => 3,
        }
    }

    fn from_index(i: i32) -> Self {
        match (i + 3) % 3 {
            0 => Self::Rock,
            1 => Self::Paper,
            2 => Self::Scissors,
            _ => unreachable!(),
        }
    }

    fn get_index(&self) -> i32 {
        match self {
            RockPaperScissors::Rock => 0,
            RockPaperScissors::Paper => 1,
            RockPaperScissors::Scissors => 2,
        }
    }

    fn get_for_result(&self, result: &str) -> Result<Self, ParseError> {
        match result {
            "X" => Ok(RockPaperScissors::from_index(self.get_index() - 1)),
            "Y" => Ok(*self),
            "Z" => Ok(RockPaperScissors::from_index(self.get_index() + 1)),
            _ => Err(ParseError(format!("cannot parse {result}"))),
        }
    }
}

struct Match {
    enemy: RockPaperScissors,
    me: RockPaperScissors,
}

impl Match {
    fn get_match_results(&self) -> u32 {
        match (self.enemy, self.me) {
            (RockPaperScissors::Rock, RockPaperScissors::Rock) => 3,
            (RockPaperScissors::Rock, RockPaperScissors::Paper) => 6,
            (RockPaperScissors::Rock, RockPaperScissors::Scissors) => 0,
            (RockPaperScissors::Paper, RockPaperScissors::Rock) => 0,
            (RockPaperScissors::Paper, RockPaperScissors::Paper) => 3,
            (RockPaperScissors::Paper, RockPaperScissors::Scissors) => 6,
            (RockPaperScissors::Scissors, RockPaperScissors::Rock) => 6,
            (RockPaperScissors::Scissors, RockPaperScissors::Paper) => 0,
            (RockPaperScissors::Scissors, RockPaperScissors::Scissors) => 3,
        }
    }

    fn get_points(&self) -> u32 {
        self.me.get_points() + self.get_match_results()
    }
}

impl FromStr for Match {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let vec = s.split(' ').collect::<Vec<&str>>();
        if vec.len() != 2 {
            return Err(ParseError(format!("could not split line: {s}")));
        }

        let enemy = RockPaperScissors::from_str(vec[0])?;

        Ok(Match {
            enemy,
            me: enemy.get_for_result(vec[1])?,
        })
    }
}

fn main() {
    let input = common::read_file("data/input.txt").expect("File could not be read");

    let lines: Vec<&str> = input.split('\n').collect();

    let result = lines
        .iter()
        .filter(|line| !line.is_empty())
        .map(|line| Match::from_str(line).expect("Could not parse line"))
        .map(|m| m.get_points());

    println!("{:?}", result.sum::<u32>());
}
