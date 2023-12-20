use std::collections::{HashMap, VecDeque};
use std::mem::MaybeUninit;

enum GateType {
    Broadcaster,
    FlipFlop(bool),
    Conjunction(Vec<bool>),
}

impl GateType {
    fn trigger(&mut self, input: bool, input_idx: usize) -> Option<bool> {
        match self {
            GateType::Broadcaster => Some(input),
            GateType::FlipFlop(state) => {
                // flipflops have only a single input
                // debug_assert_eq!(input_idx, 0);
                if !input {
                    *state = !*state;
                    Some(*state)
                } else {
                    None
                }
            }
            GateType::Conjunction(inputs) => {
                inputs[input_idx] = input;
                Some(!inputs.iter().all(|i| *i))
            }
        }
    }
}

fn get_gates(s: &str) -> (&str, Vec<&str>) {
    let idx = s.find(" -> ").unwrap();
    let name = &s[..idx];
    (name, s[idx + 4..].split(", ").collect())
}

struct GateTarget<'a> {
    gate: &'a str,
    input_index: usize,
}

struct Gate<'a> {
    ty: GateType,
    targets: Vec<GateTarget<'a>>,
}

type GatesMap<'a> = HashMap<&'a str, Gate<'a>>;

struct GateMetadata<'a> {
    ty: Option<char>,
    inputs: usize,
    targets: MaybeUninit<Vec<GateTarget<'a>>>,
}

impl<'a> Default for GateMetadata<'a> {
    fn default() -> Self {
        Self {
            ty: None,
            inputs: 0,
            targets: MaybeUninit::uninit(),
        }
    }
}

fn parse_gates(s: &str) -> GatesMap {
    let mut gates: HashMap<&str, GateMetadata> = HashMap::new();

    for line in s.lines() {
        let (name, targets) = get_gates(line);
        let (ty, name) = if line.starts_with("broadcaster") {
            ('b', name)
        } else {
            let ty = name.chars().next().unwrap();
            let name = &name[1..];

            (ty, name)
        };
        let entry = gates.entry(name).or_default();
        entry.ty = Some(ty);
        let mut target_indices = Vec::with_capacity(targets.len());

        for t in targets {
            let entry = gates.entry(t).or_default();
            target_indices.push(GateTarget {
                gate: t,
                input_index: entry.inputs,
            });
            entry.inputs += 1;
        }

        gates.get_mut(name).unwrap().targets = MaybeUninit::new(target_indices);
    }

    gates
        .into_iter()
        .flat_map(
            |(
                name,
                GateMetadata {
                    ty,
                    inputs,
                    targets,
                },
            )| {
                let ty = match ty {
                    Some('b') => GateType::Broadcaster,
                    Some('%') => GateType::FlipFlop(false),
                    Some('&') => GateType::Conjunction(vec![false; inputs]),
                    None => return None,
                    _ => unreachable!(),
                };
                Some((
                    name,
                    Gate {
                        ty,
                        targets: unsafe { targets.assume_init() },
                    },
                ))
            },
        )
        .collect::<HashMap<_, _>>()
}

pub fn part1(s: &str) -> u64 {
    let mut gates = parse_gates(s);

    let mut high_pulses = 0;
    let mut low_pulses = 0;

    for _ in 0..1000 {
        let mut worklist = VecDeque::new();
        worklist.push_back(("broadcaster", false, 0usize));

        while let Some((name, pulse, index)) = worklist.pop_front() {
            if pulse {
                high_pulses += 1;
            } else {
                low_pulses += 1;
            }

            if let Some(Gate { ty, targets }) = gates.get_mut(name) {
                if let Some(new_pulse) = ty.trigger(pulse, index) {
                    for GateTarget { gate, input_index } in targets {
                        worklist.push_back((gate, new_pulse, *input_index));
                    }
                }
            }
        }
    }

    high_pulses * low_pulses
}

pub fn part2(s: &str) -> u64 {
    let mut gates = parse_gates(s);

    let conjunction = gates
        .iter()
        .filter_map(move |(k, Gate { targets, .. })| {
            if targets.iter().any(|t| t.gate == "rx") {
                Some(*k)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    assert_eq!(conjunction.len(), 1);
    let conjunction = conjunction[0];
    let cycle_names = gates
        .iter()
        .filter_map(move |(k, Gate { targets, .. })| {
            if targets.iter().any(|t| t.gate == conjunction) {
                Some(*k)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    let mut cycle_lengths = HashMap::new();

    let mut i = 0u64;
    'outer: loop {
        i += 1;
        let mut worklist = VecDeque::new();
        worklist.push_back(("broadcaster", false, 0usize));

        while let Some((name, pulse, index)) = worklist.pop_front() {
            if cycle_names.contains(&name) && !pulse {
                cycle_lengths.insert(name, i);
                if cycle_lengths.len() == cycle_names.len() {
                    break 'outer;
                }
            }

            if let Some(Gate { ty, targets }) = gates.get_mut(name) {
                if let Some(new_pulse) = ty.trigger(pulse, index) {
                    for GateTarget { gate, input_index } in targets {
                        worklist.push_back((gate, new_pulse, *input_index));
                    }
                }
            }
        }
    }

    cycle_lengths.values().cloned().fold(1, num::integer::lcm)
}

#[cfg(test)]
mod tests {
    use crate::{part1, part2};

    #[test]
    fn p1() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part1(input), 869395600);
    }

    #[test]
    fn p2() {
        let input = include_str!("../input/input.txt");
        assert_eq!(part2(input), 232605773145467);
    }
}
