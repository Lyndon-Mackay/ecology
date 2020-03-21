extern crate rand;

use rand::rngs::ThreadRng;
use rand::Rng;

use std::env;

use std::cmp::Ordering;
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

struct Census {
    tree_count: u32,
    bear_count: u32,
    lumberjack_count: u32,
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
    fn get_terrain_counts(&self) -> Census {
        self.layout.iter().fold(
            Census {
                tree_count: 0,
                bear_count: 0,
                lumberjack_count: 0,
            },
            |acc, x| match x {
                ForestFeature::Bear(_) => Census {
                    tree_count: acc.tree_count,
                    bear_count: acc.bear_count + 1,
                    lumberjack_count: acc.lumberjack_count,
                },
                ForestFeature::LumberJack(_) => Census {
                    tree_count: acc.tree_count,
                    bear_count: acc.bear_count,
                    lumberjack_count: acc.lumberjack_count + 1,
                },
                ForestFeature::Tree(_) => Census {
                    tree_count: acc.tree_count + 1,
                    ..acc
                },
                ForestFeature::BearTree(_, _) => Census {
                    tree_count: acc.tree_count + 1,
                    bear_count: acc.bear_count + 1,
                    lumberjack_count: acc.lumberjack_count,
                },
                ForestFeature::LumberSeedling(_, _) => Census {
                    tree_count: acc.tree_count + 1,
                    bear_count: acc.bear_count,
                    lumberjack_count: acc.lumberjack_count + 1,
                },
                ForestFeature::Empty => acc,
            },
        )
    }

    fn get_lumberjack_spawnable_locations(&self) -> Vec<(usize, usize)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::Empty => Some((i / self.size, i % self.size)),
                ForestFeature::Tree(t) => match t {
                    FloraVariant::Sapling(_) => Some((i / self.size, i % self.size)),
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }
    fn get_bear_spawnable_locations(&self) -> Vec<(usize, usize)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::Empty => Some((i / self.size, i % self.size)),
                ForestFeature::Tree(_) => Some((i / self.size, i % self.size)),

                _ => None,
            })
            .collect()
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
    fn get_all_lumberjacks_locations(&self) -> Vec<(usize, usize)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::LumberJack(_) => Some((i / self.size, i % self.size)),
                ForestFeature::LumberSeedling(_, _) => Some((i / self.size, i % self.size)),
                _ => None,
            })
            .collect()
    }
    fn get_all_bear_locations(&self) -> Vec<(usize, usize)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::Bear(_) => Some((i / self.size, i % self.size)),
                ForestFeature::BearTree(_, _) => Some((i / self.size, i % self.size)),
                _ => None,
            })
            .collect()
    }
    fn get_moveable_lumberjacks(&self) -> Vec<(usize, usize, Woodcutter)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::LumberJack(l) if !l.finished_moving => {
                    Some((i / self.size, i % self.size, l))
                }
                ForestFeature::LumberSeedling(l, _) if !l.finished_moving => {
                    Some((i / self.size, i % self.size, l))
                }
                _ => None,
            })
            .collect()
    }

    fn get_moveable_bears(&self) -> Vec<(usize, usize, BearInfo)> {
        self.layout
            .iter()
            .enumerate()
            .filter_map(|(i, x)| match *x {
                ForestFeature::Bear(b) if !b.finished_moving => {
                    Some((i / self.size, i % self.size, b))
                }
                ForestFeature::BearTree(b, _) if !b.finished_moving => {
                    Some((i / self.size, i % self.size, b))
                }
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
        returned_vec
    }
    fn get_bear_destinations(&self, x: usize, y: usize) -> Vec<(usize, usize)> {
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
                    Some(ForestFeature::Empty)
                    | Some(ForestFeature::Tree(_))
                    | Some(ForestFeature::LumberJack(_))
                    | Some(ForestFeature::LumberSeedling(_, _)) => {
                        returned_vec.push((new_x, new_y))
                    }
                    _ => (),
                };
            }
        }
        returned_vec
    }
    fn add_lumberjacks(&mut self, new_locations: Vec<(usize, usize)>) {
        for position in new_locations {
            let (new_x, new_y) = position;

            let new_loc = self.get(new_x, new_y).unwrap();
            match new_loc {
                ForestFeature::Empty => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::LumberJack(Woodcutter::new());
                }
                ForestFeature::Tree(l) => match l {
                    FloraVariant::Sapling(a) => {
                        self.layout[new_x * self.size + new_y] =
                            ForestFeature::LumberSeedling(Woodcutter::new(), *a);
                    }
                    _ => panic!("spawning lumberJack invalid tree"),
                },
                _ => panic!("spawning lumberJack invalid location"),
            }
        }
    }
    fn add_bear(&mut self, new_location: (usize, usize)) {
        let (new_x, new_y) = new_location;

        let new_loc = self.get(new_x, new_y).unwrap();
        match new_loc {
            ForestFeature::Empty => {
                self.layout[new_x * self.size + new_y] = ForestFeature::Bear(BearInfo::new());
            }
            ForestFeature::Tree(l) => {
                self.layout[new_x * self.size + new_y] =
                    ForestFeature::BearTree(BearInfo::new(), *l)
            }
            _ => panic!("spawning lumberJack invalid location"),
        }
    }
    fn remove_lumberjack(&mut self, location: (usize, usize)) {
        let (new_x, new_y) = location;

        let new_loc = self.get(new_x, new_y).unwrap();
        match new_loc {
            ForestFeature::LumberJack(_) => {
                self.layout[new_x * self.size + new_y] = ForestFeature::Empty;
            }
            ForestFeature::LumberSeedling(_, s) => {
                self.layout[new_x * self.size + new_y] =
                    ForestFeature::Tree(FloraVariant::Sapling(*s))
            }
            _ => panic!("spawning lumberJack invalid location"),
        }
    }
    fn remove_bear(&mut self, location: (usize, usize)) {
        let (new_x, new_y) = location;

        let new_loc = self.get(new_x, new_y).unwrap();
        match new_loc {
            ForestFeature::Bear(_) => {
                self.layout[new_x * self.size + new_y] = ForestFeature::Empty;
            }
            ForestFeature::BearTree(_, t) => {
                self.layout[new_x * self.size + new_y] = ForestFeature::Tree(*t)
            }
            _ => panic!("spawning lumberJack invalid location"),
        }
    }
    fn plant_sapling(&mut self, x: usize, y: usize) {
        let new_loc = self.layout[x * self.size + y];

        if let ForestFeature::Empty = new_loc {
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
        let old_loc = self.get(old_x, old_y).unwrap();

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
        let new_loc = self.get(new_x, new_y).unwrap();

        match new_loc {
            ForestFeature::Empty => {
                self.layout[new_x * self.size + new_y] = ForestFeature::LumberJack(wood);
            }
            ForestFeature::Tree(l) => match l {
                FloraVariant::Sapling(a) => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::LumberSeedling(wood, *a);
                }
                FloraVariant::Tree(_) => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::LumberJack(Woodcutter {
                            lumber_collected: wood.lumber_collected + 1,
                            finished_moving: true,
                        });
                }
                FloraVariant::Elder(_) => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::LumberJack(Woodcutter {
                            lumber_collected: wood.lumber_collected + 2,
                            finished_moving: true,
                        });
                }
            },
            _ => panic!("moved lumber to invalid position"),
        }
    }

    fn move_bear(
        &mut self,
        new_x: usize,
        new_y: usize,
        bear: BearInfo,
        old_x: usize,
        old_y: usize,
    ) -> Option<Woodcutter> {
        let old_loc = self.get(old_x, old_y).unwrap();

        match old_loc {
            ForestFeature::Bear(_) => self.layout[old_x * self.size + old_y] = ForestFeature::Empty,
            ForestFeature::BearTree(_, tree) => {
                self.layout[old_x * self.size + old_y] = match tree {
                    FloraVariant::Elder(l) => ForestFeature::Tree(FloraVariant::Elder(*l)),
                    FloraVariant::Tree(l) => ForestFeature::Tree(FloraVariant::Tree(*l)),
                    FloraVariant::Sapling(sap) => ForestFeature::Tree(FloraVariant::Sapling(*sap)),
                }
            }
            old_place => panic!(
                "bear disappered {:?} {} {},{},{}",
                old_place, old_x, old_y, new_x, new_y
            ),
        }

        let new_loc = self.get(new_x, new_y).unwrap();

        match new_loc {
            ForestFeature::Empty => {
                self.layout[new_x * self.size + new_y] = ForestFeature::Bear(bear);
                None
            }
            ForestFeature::Tree(l) => match l {
                FloraVariant::Sapling(a) => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::BearTree(bear, FloraVariant::Sapling(*a));
                    None
                }
                FloraVariant::Tree(tree) => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::BearTree(bear, FloraVariant::Tree(*tree));
                    None
                }
                FloraVariant::Elder(tree) => {
                    self.layout[new_x * self.size + new_y] =
                        ForestFeature::BearTree(bear, FloraVariant::Elder(*tree));
                    None
                }
            },
            ForestFeature::LumberJack(l) => {
                let lumber_copy = *l;
                self.layout[new_x * self.size + new_y] = ForestFeature::Bear(BearInfo {
                    finished_moving: true,
                });
                Some(lumber_copy)
            }
            ForestFeature::LumberSeedling(l, sap) => {
                let lumber_copy = *l;
                self.layout[new_x * self.size + new_y] = ForestFeature::BearTree(
                    BearInfo {
                        finished_moving: true,
                    },
                    FloraVariant::Sapling(*sap),
                );
                Some(lumber_copy)
            }

            _ => panic!("moved bear to invalid position"),
        }
    }
    fn allow_moves(&mut self) {
        for x in self.layout.iter_mut() {
            match x {
                ForestFeature::Bear(b) => b.finished_moving = false,
                ForestFeature::BearTree(b, _) => b.finished_moving = false,
                ForestFeature::LumberJack(l) => {
                    l.finished_moving = false;
                    l.lumber_collected = 0
                }
                ForestFeature::LumberSeedling(l, _) => {
                    l.finished_moving = false;
                    l.lumber_collected = 0
                }
                _ => (),
            }
        }
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

impl fmt::Display for Census {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        writeln!(
            f,
            "Trees:{} Bears:{} Lumberjacks:{}",
            self.tree_count, self.bear_count, self.lumberjack_count
        )?;

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
        let mut annual_wood_chop = 0;
        let mut annual_mualing = 0;
        let mut annual_sapling_plant = 0;
        for month in 1..12 {
            simulated_forest.age_trees();

            let saplings_planted_this_month = process_spawning(&mut simulated_forest, &mut rng);

            let mut mauled_lumberjacks = Vec::with_capacity(size * size);
            for move_phase in 0..5 {
                if move_phase < 3 {
                    process_lumberjacks(&mut simulated_forest, &mut rng);
                }
                mauled_lumberjacks.append(&mut process_bear(&mut simulated_forest, &mut rng));
            }

            let mauled_lumberjacks_this_month = mauled_lumberjacks.len();
            let wood_chopped_this_month = mauled_lumberjacks
                .into_iter()
                .fold(0, |acc, x| acc + x.lumber_collected)
                + simulated_forest.amount_of_wood_chopped();
            annual_wood_chop += wood_chopped_this_month;
            annual_mualing += mauled_lumberjacks_this_month as u32;
            annual_sapling_plant += saplings_planted_this_month;
            simulated_forest.allow_moves();
            println!("{}", simulated_forest);
            println!(
                "month {} year {}, {}: units of wood chopped this month {}: lumberJacks mauled {}: saplings planted",
                month,year,wood_chopped_this_month, mauled_lumberjacks_this_month, saplings_planted_this_month
            );
            println!("{:-<1$}", "", size * 2);
        }
        let censare = populate(
            &mut simulated_forest,
            &mut rng,
            annual_wood_chop,
            annual_mualing,
        );
        println!("yearly census {}", censare);
        println!(
            "year {}, wood chopped this year:{} ,lumberJacks mauled:{} ,saplings planted:{} ",
            year, annual_wood_chop, annual_mualing, annual_sapling_plant
        );
        println!("{:_<1$}", "", size * 2);

        if censare.tree_count == 0 {
            println!("The once great forest has disappeared");
            std::process::exit(0);
        }
    }
}

fn process_spawning(simulated_forest: &mut Forest, rng: &mut ThreadRng) -> u32 {
    let mature_tress = simulated_forest.get_mature_trees();

    let mut num_saplings_planted = 0;

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
                num_saplings_planted += 1;
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
                num_saplings_planted += 1;
            }
            _ => continue,
        }
    }
    num_saplings_planted
}
fn process_lumberjacks(simulated_forest: &mut Forest, rng: &mut ThreadRng) {
    let lumber_jack_locations = simulated_forest.get_moveable_lumberjacks();

    for l in lumber_jack_locations {
        let (old_x, old_y, wood) = l;
        let new_destinations = simulated_forest.get_lumber_jack_destinations(old_x, old_y);
        if new_destinations.is_empty() {
            continue;
        }
        let (new_x, new_y) = new_destinations
            .get(rng.gen_range(0, new_destinations.len()))
            .unwrap();
        simulated_forest.move_lumberjack(*new_x, *new_y, wood, old_x, old_y);
    }
}
fn process_bear(simulated_forest: &mut Forest, rng: &mut ThreadRng) -> Vec<Woodcutter> {
    let bear_locations = simulated_forest.get_moveable_bears();

    let mut maulings = Vec::with_capacity(bear_locations.len());

    for l in bear_locations {
        let (old_x, old_y, bear) = l;
        let new_destinations = simulated_forest.get_bear_destinations(old_x, old_y);
        if new_destinations.is_empty() {
            continue;
        }
        let (new_x, new_y) = new_destinations
            .get(rng.gen_range(0, new_destinations.len()))
            .unwrap();
        let result = simulated_forest.move_bear(*new_x, *new_y, bear, old_x, old_y);
        maulings.push(result);
    }
    maulings.into_iter().filter_map(|x| x).collect()
}

fn populate(
    simulated_forest: &mut Forest,
    rng: &mut ThreadRng,
    wood_collected: u32,
    maul_incidents: u32,
) -> Census {
    let censare = simulated_forest.get_terrain_counts();

    let delta_lumber = populate_lumberjacks(
        simulated_forest,
        rng,
        wood_collected,
        censare.lumberjack_count,
    );

    let delta_bears = populate_bears(simulated_forest, rng, maul_incidents);

    Census {
        bear_count: censare.bear_count + delta_bears,
        lumberjack_count: (censare.lumberjack_count as i32 + delta_lumber) as u32,
        ..censare
    }
}

fn populate_lumberjacks(
    simulated_forest: &mut Forest,
    rng: &mut ThreadRng,
    wood_collected: u32,
    lumberjack_count: u32,
) -> i32 {
    let delta_num_lumberjacks = if lumberjack_count == 0 {
        1
    } else if lumberjack_count > wood_collected {
        -1
    } else {
        (wood_collected / lumberjack_count) as i32
    };

    match delta_num_lumberjacks.cmp(&0) {
        Ordering::Equal => {}
        Ordering::Greater => {
            let mut empty_locations = simulated_forest.get_lumberjack_spawnable_locations();

            let mut lumber_spawn_sites = Vec::with_capacity(delta_num_lumberjacks as usize);

            for _x in 0..delta_num_lumberjacks {
                let new_lumber_location =
                    empty_locations.remove(rng.gen_range(0, empty_locations.len()));
                lumber_spawn_sites.push(new_lumber_location);
            }
            simulated_forest.add_lumberjacks(lumber_spawn_sites);
        }
        Ordering::Less => {
            let mut cullable_lumberjacks = simulated_forest.get_all_lumberjacks_locations();
            let culled_lumber_jack =
                cullable_lumberjacks.remove(rng.gen_range(0, cullable_lumberjacks.len()));
            simulated_forest.remove_lumberjack(culled_lumber_jack);
        }
    }
    delta_num_lumberjacks
}
fn populate_bears(simulated_forest: &mut Forest, rng: &mut ThreadRng, maul_incidents: u32) -> u32 {
    match maul_incidents.cmp(&0) {
        Ordering::Equal | Ordering::Less => {
            let mut bear_spawn_spots = simulated_forest.get_bear_spawnable_locations();

            let new_bear_location =
                bear_spawn_spots.remove(rng.gen_range(0, bear_spawn_spots.len()));

            simulated_forest.add_bear(new_bear_location);
            1
        }
        Ordering::Greater => {
            let mut cullable_bears = simulated_forest.get_all_bear_locations();
            let culled_lumber_jack = cullable_bears.remove(rng.gen_range(0, cullable_bears.len()));
            simulated_forest.remove_bear(culled_lumber_jack);
            0
        }
    }
}
