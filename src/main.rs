extern crate rand;

use rand::rngs::ThreadRng;
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

enum MatureTree {
    Tree,
    Elder,
}

struct MatureTreeLocation {
    m_tree: MatureTree,
    x: usize,
    y: usize,
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
    fn get_mature_trees(&self) -> Vec<MatureTreeLocation> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::Tree(t) => match t {
                    FloraVariant::Tree(_) => Some(MatureTreeLocation {
                        m_tree: MatureTree::Tree,
                        x: i / self.size,
                        y: i % self.size,
                    }),
                    FloraVariant::Elder(_) => Some(MatureTreeLocation {
                        m_tree: MatureTree::Elder,
                        x: i / self.size,
                        y: i % self.size,
                    }),
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }
    fn get_lumberjacks(&self) -> Vec<(usize, usize, Woodcutter)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::LumberJack(l) => Some((i / self.size, i % self.size, l)),
                ForestFeature::LumberSeedling(l, _) => Some((i / self.size, i % self.size, l)),
                _ => None,
            })
            .collect()
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
        for delta_x in [-1, 0, 1].iter().cloned() {
            for delta_y in [-1, 0, 1].iter().cloned() {
                let new_x = delta_x + x as i32;
                let new_y = delta_y + y as i32;

                if (delta_x == 0 && delta_y == 0)
                    || new_x as usize >= self.size
                    || new_y as usize >= self.size
                    || new_x < 0
                    || new_y < 0
                {
                    continue;
                }

                let new_x = new_x as usize;
                let new_y = new_y as usize;

                let neighbour = self.get(new_x, new_y);

                if let Some(ForestFeature::Empty) = neighbour {
                    returned_vec.push((new_x, new_y))
                }
            }
        }
        returned_vec
    }
    fn get_lumber_jack_destinations(&self, x: usize, y: usize) -> Vec<(usize, usize)> {
        let mut returned_vec = Vec::with_capacity(8);
        for delta_x in [-1, 0, 1].iter().cloned() {
            for delta_y in [-1, 0, 1].iter().cloned() {
                let new_x = delta_x + x as i32;
                let new_y = delta_y + y as i32;

                if (delta_x == 0 && delta_y == 0)
                    || new_x as usize >= self.size
                    || new_y as usize >= self.size
                    || new_x < 0
                    || new_y < 0
                {
                    continue;
                }

                let new_x = new_x as usize;
                let new_y = new_y as usize;

                let neighbour = self.get(new_x, new_y);

                match neighbour {
                    Some(ForestFeature::Empty) | Some(ForestFeature::Tree(_)) => {
                        returned_vec.push((new_x, new_y))
                    }
                    _ => (),
                };
            }
        }
        println!("{:?} {} {}", returned_vec, x, y);
        returned_vec
    }
    fn plant_sapling(&mut self, x: usize, y: usize) {
        let new_loc = self.layout[x * self.size + y];

        if let ForestFeature::Empty = new_loc {
            println!("plant at {} {}", x, y);
            self.layout[x * self.size + y] =
                ForestFeature::Tree(FloraVariant::Sapling(Seedling { current_age: 0 }));
        } else {
            panic!("planting in a non empty zone {} {} {:?}", x, y, new_loc);
        }
    }
    fn move_lumberjack(
        &mut self,
        new_x: usize,
        new_y: usize,
        wood: Woodcutter,
        old_x: usize,
        old_y: usize,
    ) {
        let new_loc = self.get(new_x, new_y).unwrap();

        println!("pre move\n{}", self);
        match new_loc {
            ForestFeature::Empty => {
                println!("move to empty");
                self.layout[new_x * self.size + new_y] = ForestFeature::LumberJack(wood);
            }
            ForestFeature::Tree(l) => {
                match l {
                    FloraVariant::Sapling(a) => {
                        println!("moved to sap");
                        self.layout[new_x * self.size + new_y] =
                            ForestFeature::LumberSeedling(wood, *a);
                    }
                    FloraVariant::Tree(_) => {
                        println!("moved to tree");
                        self.layout[new_x * self.size + new_y] =
                            ForestFeature::LumberJack(Woodcutter {
                                lumber_collected: wood.lumber_collected + 1,
                                finished_moving: true,
                            });
                    }
                    FloraVariant::Elder(_) => {
                        println!("moved to elder");
                        self.layout[new_x * self.size + new_y] =
                            ForestFeature::LumberJack(Woodcutter {
                                lumber_collected: wood.lumber_collected + 2,
                                finished_moving: true,
                            });
                    }
                };
            }
            _ => panic!("moved lumber to invalid position"),
        }

        let old_loc = self.get(old_x, old_y).unwrap();

        println!("mid move\n{}", self);
        match old_loc {
            ForestFeature::LumberJack(_) => {
                self.layout[old_x * self.size + old_y] = ForestFeature::Empty
            }
            ForestFeature::LumberSeedling(_, sap) => {
                self.layout[old_x * self.size + old_y] =
                    ForestFeature::Tree(FloraVariant::Sapling(*sap))
            }
            old_place => panic!(
                "lumberjack disappered {:?} {} {},{},{}",
                old_place, old_x, old_y, new_x, new_y
            ),
        }
        println!("post move\n{}", self);
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

    let mut rng = rand::thread_rng();
    let size = &args[1];

    let size = size.parse().expect("positive number expected");

    let mut simulated_forest = Forest::new(size);

    for year in 1..400 {
        for month in 1..12 {
            simulated_forest.age_trees();

            process_spawning(&mut simulated_forest, &mut rng);

            println!("{}", simulated_forest);

            for move_phase in 0..5 {
                if move_phase < 3 {
                    process_lumberjacks(&mut simulated_forest, &mut rng);
                }
            }

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

fn process_spawning(simulated_forest: &mut Forest, rng: &mut ThreadRng) {
    let mature_tress = simulated_forest.get_mature_trees();

    for m in mature_tress {
        let gen = rng.gen_range(0, 10);
        match m.m_tree {
            MatureTree::Tree if gen == 0 => {
                let potential_locations = simulated_forest.get_potential_spawn_sites(m.x, m.y);
                if potential_locations.is_empty() {
                    continue;
                }
                let (new_site_x, new_site_y) = potential_locations
                    .get(rng.gen_range(0, potential_locations.len()))
                    .unwrap();
                simulated_forest.plant_sapling(*new_site_x, *new_site_y);
            }
            MatureTree::Elder if gen == 0 || gen == 1 => {
                let potential_locations = simulated_forest.get_potential_spawn_sites(m.x, m.y);
                if potential_locations.is_empty() {
                    continue;
                }
                let (new_site_x, new_site_y) = potential_locations
                    .get(rng.gen_range(0, potential_locations.len()))
                    .unwrap();
                simulated_forest.plant_sapling(*new_site_x, *new_site_y);
            }
            _ => continue,
        }
    }
}
fn process_lumberjacks(simulated_forest: &mut Forest, rng: &mut ThreadRng) {
    let lumber_jack_locations = simulated_forest.get_lumberjacks();

    for l in lumber_jack_locations {
        let (old_x, old_y, wood) = l;
        let new_destinations = simulated_forest.get_lumber_jack_destinations(old_x, old_y);
        if new_destinations.is_empty() {
            continue;
        }
        let (new_x, new_y) = new_destinations
            .get(rng.gen_range(0, new_destinations.len()))
            .unwrap();
        println!("chose {} {}", new_x, new_y);
        simulated_forest.move_lumberjack(*new_x, *new_y, wood, old_x, old_y);
    }
}
