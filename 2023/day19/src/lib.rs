use std::collections::HashMap;
use std::ops::Range;

enum Action<'a> {
    Accept,
    Reject,
    Rule(&'a str),
}

enum ComparisonKind {
    Lt,
    Gt,
    Default,
}

#[derive(Copy, Clone)]
enum Field {
    X,
    A,
    M,
    S,
}

impl TryFrom<char> for Field {
    type Error = ();

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'x' => Ok(Field::X),
            'a' => Ok(Field::A),
            'm' => Ok(Field::M),
            's' => Ok(Field::S),
            _ => Err(()),
        }
    }
}

struct Rule<'a> {
    field: Field,
    cmp: ComparisonKind,
    value: u32,
    action: Action<'a>,
}

impl<'a> Rule<'a> {
    fn matches(&self, item: &Item<u32>) -> bool {
        let val = *item.get_field(self.field);
        let other = self.value;
        let (val, other) = match self.cmp {
            ComparisonKind::Lt => (val, other),
            ComparisonKind::Gt => (other, val),
            ComparisonKind::Default => return true,
        };

        val < other
    }
}

#[derive(Clone, Default)]
struct Item<T> {
    x: T,
    a: T,
    m: T,
    s: T,
}

impl<T> Item<T> {
    fn get_field(&self, field: Field) -> &T {
        match field {
            Field::X => &self.x,
            Field::A => &self.a,
            Field::M => &self.m,
            Field::S => &self.s,
        }
    }
    fn get_field_mut(&mut self, field: Field) -> &mut T {
        match field {
            Field::X => &mut self.x,
            Field::A => &mut self.a,
            Field::M => &mut self.m,
            Field::S => &mut self.s,
        }
    }
}

impl Item<u32> {
    fn value(&self) -> u64 {
        u64::from(self.x) + u64::from(self.m) + u64::from(self.a) + u64::from(self.s)
    }
}

impl Item<Range<u32>> {
    fn value(self) -> u64 {
        u64::from(self.x.end.saturating_sub(self.x.start))
            * u64::from(self.a.end.saturating_sub(self.a.start))
            * u64::from(self.m.end.saturating_sub(self.m.start))
            * u64::from(self.s.end.saturating_sub(self.s.start))
    }
}

fn parse_action(s: &str) -> Rule {
    let (field, cmp, idx, value) =
        if s.len() > 1 && (s.as_bytes()[1] as char == '<' || s.as_bytes()[1] as char == '>') {
            let field = Field::try_from(s.as_bytes()[0] as char).unwrap();
            let cmp = match s.as_bytes()[1] as char {
                '<' => ComparisonKind::Lt,
                '>' => ComparisonKind::Gt,
                _ => panic!(),
            };
            let idx = s.find(':').unwrap();
            let value = s[2..idx].parse().unwrap();

            (field, cmp, idx + 1, value)
        } else {
            (Field::A, ComparisonKind::Default, 0, 0)
        };
    let action = match &s[idx..s.len()] {
        "A" => Action::Accept,
        "R" => Action::Reject,
        s => Action::Rule(s),
    };
    Rule {
        field,
        cmp,
        value,
        action,
    }
}

fn parse_workflow(line: &str) -> (&str, Vec<Rule>) {
    let sep = line.find('{').unwrap();
    let name = &line[0..sep];
    let rules = line[sep + 1..line.len() - 1]
        .split(',')
        .map(parse_action)
        .collect();

    (name, rules)
}

fn parse_item(line: &str) -> Item<u32> {
    let line = &line[1..line.len() - 1];
    line.split(',')
        .map(|s| (s.as_bytes()[0] as char, s[2..].parse::<u32>().unwrap()))
        .fold(Item::default(), |mut item, (field, val)| {
            *item.get_field_mut(Field::try_from(field).unwrap()) = val;
            item
        })
}

pub fn part1(s: &str) -> u64 {
    let mut splits = s.split("\n\n");
    let workflows = splits
        .next()
        .unwrap()
        .lines()
        .map(parse_workflow)
        .collect::<HashMap<_, _>>();

    let mut items = splits
        .next()
        .unwrap()
        .lines()
        .map(parse_item)
        .map(|item| ("in", item))
        .collect::<Vec<_>>();

    let mut sum = 0;

    while let Some((workflow, item)) = items.pop() {
        let rules = workflows.get(workflow).unwrap();
        let action = rules
            .iter()
            .filter(|rule| rule.matches(&item))
            .map(|rule| &rule.action)
            .next()
            .unwrap();
        match action {
            Action::Accept => sum += item.value(),
            Action::Reject => {}
            Action::Rule(s) => items.push((s, item)),
        }
    }

    sum
}

pub fn part2(s: &str) -> u64 {
    let mut splits = s.split("\n\n");
    let workflows = splits
        .next()
        .unwrap()
        .lines()
        .map(parse_workflow)
        .collect::<HashMap<_, _>>();

    let mut result = 0;
    let mut worklist = vec![(
        "in",
        Item {
            x: 1..4001,
            a: 1..4001,
            m: 1..4001,
            s: 1..4001,
        },
    )];

    while let Some((workflow, mut item)) = worklist.pop() {
        let workflow = workflows.get(workflow).unwrap();

        for rule in workflow {
            let field = item.get_field(rule.field);

            let (matching_amt, not_matching_amt) = match rule.cmp {
                ComparisonKind::Lt => (
                    field.start..u32::min(field.end, rule.value),
                    u32::min(field.end, rule.value)..field.end,
                ),
                ComparisonKind::Gt => (
                    u32::min(field.end, rule.value + 1)..field.end,
                    field.start..u32::min(field.end, rule.value + 1),
                ),
                ComparisonKind::Default => (field.clone(), Range::default()),
            };

            let mut matching_item = item.clone();
            *matching_item.get_field_mut(rule.field) = matching_amt.clone();
            match rule.action {
                Action::Accept => result += matching_item.value(),
                Action::Reject => {}
                Action::Rule(s) => worklist.push((s, matching_item)),
            }

            *item.get_field_mut(rule.field) = not_matching_amt;
        }

        debug_assert_eq!(item.value(), 0);
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::{part1, part2};

    #[test]
    fn p1() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part1(input), 532551);
    }

    #[test]
    fn p2() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part2(input), 134343280273968);
    }
}
