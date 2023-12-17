use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fmt::{Display, Formatter};

type Pos = (usize, usize);

#[derive(Copy, Clone, Eq, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Display for Direction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::Up => write!(f, "^"),
            Direction::Down => write!(f, "v"),
            Direction::Left => write!(f, "<"),
            Direction::Right => write!(f, ">"),
        }
    }
}

impl Direction {
    fn left(&self) -> Self {
        match self {
            Direction::Up => Direction::Left,
            Direction::Down => Direction::Right,
            Direction::Left => Direction::Down,
            Direction::Right => Direction::Up,
        }
    }

    fn right(&self) -> Self {
        match self {
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Right => Direction::Down,
        }
    }

    fn ord(&self) -> usize {
        match self {
            Direction::Up => 0,
            Direction::Down => 1,
            Direction::Left => 2,
            Direction::Right => 3,
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
struct State {
    cost: usize,
    pos: Pos,
    straights_available: usize,
    dir: Direction,
}

impl PartialOrd<Self> for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(other.cost.cmp(&self.cost))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl State {
    fn forward(&self, width: usize, height: usize) -> Option<Self> {
        if self.straights_available == 0 {
            return None;
        }

        let (r, c) = match self.dir {
            Direction::Up => (self.pos.0 as isize - 1, self.pos.1 as isize),
            Direction::Down => (self.pos.0 as isize + 1, self.pos.1 as isize),
            Direction::Left => (self.pos.0 as isize, self.pos.1 as isize - 1),
            Direction::Right => (self.pos.0 as isize, self.pos.1 as isize + 1),
        };

        if r < 0 || r >= height as isize || c < 0 || c >= width as isize {
            None
        } else {
            Some(State {
                cost: self.cost,
                pos: (r as usize, c as usize),
                straights_available: self.straights_available - 1,
                dir: self.dir,
            })
        }
    }
}

#[inline]
fn access_cache(
    dist: &mut Vec<usize>,
    width: usize,
    height: usize,
    max_straight: usize,
    pos: Pos,
    straight: usize,
    dir: Direction,
) -> &mut usize {
    let mul1 = width * height; // for max_straight
    let mul2 = mul1 * (max_straight + 1); // for dirs
    dist.get_mut(pos.0 * width + pos.1 + mul1 * straight + dir.ord() * mul2)
        .unwrap()
}

fn dijkstra(s: &str, max_straight: usize, min_straight: usize) -> usize {
    let width = s.find('\n').unwrap();
    let height = (s.len() + 1) / (width + 1);

    let goal = (height - 1, width - 1);

    let mut dist = vec![usize::MAX; width * height * (max_straight + 1) * 4];
    let mut queue: BinaryHeap<State> = BinaryHeap::new();

    queue.push(State {
        cost: 0,
        pos: (0, 0),
        straights_available: max_straight,
        dir: Direction::Down,
    });
    queue.push(State {
        cost: 0,
        pos: (0, 0),
        straights_available: max_straight,
        dir: Direction::Right,
    });

    while let Some(state) = queue.pop() {
        if state.pos == goal {
            if state.straights_available <= max_straight - min_straight {
                return state.cost;
            } else {
                continue;
            }
        }

        if state.cost
            > *access_cache(
                &mut dist,
                width,
                height,
                max_straight,
                state.pos,
                state.straights_available,
                state.dir,
            )
        {
            // cheaper path exists
            continue;
        }

        let next_states = [
            if state.straights_available <= max_straight - min_straight {
                Some(State {
                    cost: state.cost,
                    pos: state.pos,
                    straights_available: max_straight,
                    dir: state.dir.left(),
                })
            } else {
                None
            },
            if state.straights_available <= max_straight - min_straight {
                Some(State {
                    cost: state.cost,
                    pos: state.pos,
                    straights_available: max_straight,
                    dir: state.dir.right(),
                })
            } else {
                None
            },
            Some(state),
        ];

        next_states
            .iter()
            .flatten()
            .filter_map(|s| s.forward(width, height))
            .map(|mut state| {
                state.cost +=
                    (s.as_bytes()[state.pos.0 * (width + 1) + state.pos.1] - b'0') as usize;
                state
            })
            .for_each(|state| {
                let item = access_cache(
                    &mut dist,
                    width,
                    height,
                    max_straight,
                    state.pos,
                    state.straights_available,
                    state.dir,
                );
                if state.cost < *item {
                    *item = state.cost;
                    queue.push(state);
                }
            })
    }

    unreachable!()
}

pub fn part1(s: &str) -> usize {
    dijkstra(s, 3, 0)
}

pub fn part2(s: &str) -> usize {
    dijkstra(s, 10, 4)
}
