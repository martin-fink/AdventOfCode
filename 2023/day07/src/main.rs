use anyhow::Result;
use aoc::aoc_main;
use itertools::Itertools;
use std::cmp::Ordering;

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Copy, Clone, Debug)]
enum Card {
    Joker,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    T,
    J,
    Q,
    K,
    A,
}

impl TryFrom<char> for Card {
    type Error = ();

    fn try_from(value: char) -> std::result::Result<Self, Self::Error> {
        match value {
            '2' => Ok(Self::Two),
            '3' => Ok(Self::Three),
            '4' => Ok(Self::Four),
            '5' => Ok(Self::Five),
            '6' => Ok(Self::Six),
            '7' => Ok(Self::Seven),
            '8' => Ok(Self::Eight),
            '9' => Ok(Self::Nine),
            'T' => Ok(Self::T),
            'J' => Ok(Self::J),
            'Q' => Ok(Self::Q),
            'K' => Ok(Self::K),
            'A' => Ok(Self::A),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
enum Type {
    HighCard,
    OnePair,
    TwoPair,
    ThreeKind,
    FullHouse,
    FourKind,
    FiveKind,
}

impl Type {
    fn from_cards(cards: &[(Card, usize)], jokers: usize) -> Self {
        if cards.len() == 1 {
            let (_, n) = cards[0];
            assert_eq!(n, 5);
            Self::FiveKind
        } else if cards.len() == 2 {
            let (_, n1) = cards[0];
            let (_, n2) = cards[1];
            if n1 == 4 || n2 == 4 {
                Self::FourKind
            } else {
                Self::FullHouse
            }
        } else if cards.len() == 3 {
            let (_, n1) = cards[0];
            let (_, n2) = cards[1];
            let (_, n3) = cards[2];
            if n1 == 3 || n2 == 3 || n3 == 3 {
                Self::ThreeKind
            } else {
                Self::TwoPair
            }
        } else if cards.len() == 4 {
            let (_, n1) = cards[0];
            let (_, n2) = cards[1];
            let (_, n3) = cards[2];
            let (_, n4) = cards[3];
            assert!(n1 == 2 || n2 == 2 || n3 == 2 || n4 == 2);
            Self::OnePair
        } else {
            assert_eq!(cards.len(), 5);
            Self::HighCard
        }
        .promote(jokers)
    }

    fn promote(&self, jokers: usize) -> Self {
        let mut result = *self;

        for _ in 0..jokers {
            result = match result {
                Type::HighCard => Type::OnePair,
                Type::OnePair => Type::ThreeKind,
                Type::TwoPair => Type::FullHouse,
                Type::ThreeKind | Type::FullHouse => Type::FourKind,
                Type::FourKind | Type::FiveKind => return Type::FiveKind,
            };
        }

        result
    }
}

#[derive(Debug)]
struct Hand {
    orig_cards: Vec<Card>,
    ty: Type,
}

impl Hand {
    fn new(cards: Vec<Card>) -> Self {
        assert_eq!(cards.len(), 5);

        let mut result = Vec::with_capacity(5);
        let mut jokers = 0usize;

        for (k, v) in &cards.iter().sorted().group_by(|s| **s) {
            if k == Card::Joker {
                assert_eq!(jokers, 0);
                jokers = v.count();
                for _ in 0..jokers {
                    result.push((k, 0));
                }
            } else {
                result.push((k, v.count()))
            }
        }

        Self {
            orig_cards: cards,
            ty: Type::from_cards(&result, jokers),
        }
    }

    fn orig_cards(&self) -> &[Card] {
        &self.orig_cards
    }
}

impl Eq for Hand {}

impl PartialEq<Self> for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        let ord1 = self.ty.cmp(&other.ty);
        if ord1 != Ordering::Equal {
            return ord1;
        }

        let cards1 = self.orig_cards();
        let cards2 = other.orig_cards();

        assert_eq!(cards1.len(), cards2.len());

        for (card1, card2) in cards1.iter().zip(cards2.iter()) {
            let ord = card1.cmp(card2);
            if ord != Ordering::Equal {
                return ord;
            }
        }

        Ordering::Equal
    }
}

fn main() -> Result<()> {
    let result = aoc_main(part1, part2)??;

    println!("{result}");

    Ok(())
}

fn parse_line(line: &str, jokers: bool) -> (Hand, u32) {
    let splits = line.split(' ').collect::<Vec<_>>();
    let cards = splits[0]
        .chars()
        .map(|c| Card::try_from(c).unwrap())
        .map(|c| {
            if jokers && c == Card::J {
                Card::Joker
            } else {
                c
            }
        })
        .collect::<Vec<_>>();
    let hand = Hand::new(cards);

    (hand, splits[1].parse().unwrap())
}

fn get_win(s: &str, jokers: bool) -> u32 {
    let vec = s
        .lines()
        .map(|s| parse_line(s, jokers))
        .sorted_by(|(h1, _), (h2, _)| h1.cmp(h2))
        .enumerate();

    

    vec.map(|(n, (_, bid))| (n as u32 + 1) * bid).sum::<u32>()
}

fn part1(s: &str) -> Result<u32> {
    Ok(get_win(s, false))
}

fn part2(s: &str) -> Result<u32> {
    Ok(get_win(s, true))
}
