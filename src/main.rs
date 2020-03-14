extern crate rand;

use rand::Rng;

use std::env;

use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct Seedling {
    current_age: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FloraVariant {
    Sapling(Seedling),
    Tree(u32),
    Elder(u32),
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct Woodcutter {
    lumber_collected: u32,
    finished_moving: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
struct BearInfo {
    finished_moving: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ForestFeature {
    Empty,
    Bear(BearInfo),
    LumberJack(Woodcutter),
    Tree(FloraVariant),
    LumberSeedling(Woodcutter, Seedling),
    BearTree(BearInfo, FloraVariant),
}

pub trait Growing<T> {
    fn age(&self) -> T;
}

impl Growing<FloraVariant> for FloraVariant {
    fn age(&self) -> FloraVariant {
        match *self {
            FloraVariant::Sapling(seed) if seed.current_age < 12 => {
                FloraVariant::Sapling(Seedling {
                    current_age: seed.current_age + 1,
                })
            }
            FloraVariant::Sapling(seed) => FloraVariant::Tree(seed.current_age + 1),
            FloraVariant::Tree(age) if age < 119 => FloraVariant::Tree(age + 1),
            FloraVariant::Tree(age) => FloraVariant::Elder(age + 1),
            FloraVariant::Elder(age) => FloraVariant::Elder(age + 1),
        }
    }
}
impl Growing<ForestFeature> for ForestFeature {
    fn age(&self) -> ForestFeature {
        match self {
            ForestFeature::Tree(tre_detail) => ForestFeature::Tree(tre_detail.age()),
            ForestFeature::LumberSeedling(wood_cut, seed) => ForestFeature::LumberSeedling(
                *wood_cut,
                Seedling {
                    current_age: seed.current_age + 1,
                },
            ),
            ForestFeature::BearTree(bear, tree_info) => {
                ForestFeature::BearTree(*bear, tree_info.age())
            }
            a => *a,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Forest {
    layout: Vec<ForestFeature>,
    size: usize,
}

impl Woodcutter {
    fn new() -> Self {
        Woodcutter {
            lumber_collected: 0,
            finished_moving: false,
        }
    }
}
impl BearInfo {
    fn new() -> Self {
        BearInfo {
            finished_moving: false,
        }
    }
}

impl Forest {
    fn new(size: usize) -> Self {
        let mut rng = rand::thread_rng();
        Forest {
            layout: (0..(size * size))
                .map(|_| match rng.gen_range(0, 100) {
                    0..=2 => ForestFeature::Bear(BearInfo::new()),
                    3..=53 => ForestFeature::Tree(FloraVariant::Tree(12)),
                    54..=64 => ForestFeature::LumberJack(Woodcutter::new()),
                    _ => ForestFeature::Empty,
                })
                .collect(),
            size,
        }
    }
    fn get(&self, x: usize, y: usize) -> Option<&ForestFeature> {
        self.layout.get(x * self.size + y)
    }
    fn age_trees(&mut self) {
        self.layout = self.layout.iter().map(|x| x.age()).collect();
    }
    fn amount_of_wood_chopped(&self) -> u32 {
        self.layout
            .iter()
            .filter_map(|x| match x {
                ForestFeature::LumberJack(l) => Some(l.lumber_collected),
                ForestFeature::LumberSeedling(l, _) => Some(l.lumber_collected),
                _ => None,
            })
            .sum()
    }
    fn get_potential_spawn_sites(&self, x: usize, y: usize) -> Vec<(usize, usize)> {
        let mut returned_vec = Vec::with_capacity(8);
        for delta_x in [self.size as u32 - 1, 0, 1].iter().cloned() {
            for delta_y in [self.size as u32 - 1, 0, 1].iter().cloned() {
                let new_x = delta_x as usize + x;
                let new_y = delta_y as usize + y;

                if (x as u32 + delta_x == 0 && y as u32 + delta_y == 0)
                    || new_x >= self.size
                    || new_y >= self.size
                {
                    continue;
                }

                let neighbour = self.get(new_x, new_y);

                if let Some(ForestFeature::Empty) = neighbour {
                    returned_vec.push((new_x, new_y))
                }
            }
        }
        returned_vec
    }
}

impl fmt::Display for Forest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        for line in self.layout.as_slice().chunks(self.size) {
            for &terrain in line {
                let symbol = match terrain {
                    ForestFeature::Empty => '⬛',
                    ForestFeature::Tree(t) => match t {
                        FloraVariant::Sapling(_) => '🎄',
                        FloraVariant::Tree(_) => '🌲',
                        FloraVariant::Elder(_) => '🌳',
                    },
                    ForestFeature::LumberJack(_) => '👷',
                    ForestFeature::Bear(_) => '🐻',
                    ForestFeature::BearTree(_, _) => '🍯',
                    ForestFeature::LumberSeedling(_, _) => '🌴',
                };
                write!(f, "{}", symbol)?;
            }
            writeln!(f)?;
        }

        Ok(())
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Should have one argument the size of the map");
    }

    let size = &args[1];

    let size = size.parse().expect("positive number expected");

    let mut simulated_forest = Forest::new(size);

    for year in (1..400) {
        for month in (1..12) {
            simulated_forest.age_trees();
            println!("{}", simulated_forest);
            println!("{:-<1$}", "", size * 2);
        }
        println!(
            "wood chopped this year {}",
            simulated_forest.amount_of_wood_chopped()
        );
        println!("{:-<1$}", "", size * 2);
    }

    println!("Hello, world!");
}
