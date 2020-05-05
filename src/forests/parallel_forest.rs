use rand::Rng;

use crate::terrain::*;
use rayon::prelude::*;

use std::fmt;
#[derive(Clone, Debug)]
pub struct Forest {
    layout: Vec<ForestFeature>,
    size: usize,
}

impl Forest {
    pub fn new(size: usize) -> Self {
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

    ///
    /// takes an x and y cordiante gives a copy the resulting forestfeature
    pub fn get(&self, x: usize, y: usize) -> ForestFeature {
        self.layout[x * self.size + y]
    }

    ///
    /// Converts an absoluute location into
    /// the projected x,y cordinate
    pub fn get_x_y(&self, i: usize) -> (usize, usize) {
        (i / self.size, i % self.size)
    }

    pub fn get_absolute_cordinate(&self, x: usize, y: usize) -> usize {
        x * self.size + y
    }

    ///
    /// Takes an x,y coridnate and a feature to assign then
    /// overwrites the location with the new feature
    ///
    pub fn assign_at(&mut self, x: usize, y: usize, f: ForestFeature) {
        self.layout[x * self.size + y] = f;
    }

    ///
    /// Returns a census struct that
    /// has the populations of all
    /// the forestfeatures
    ///
    ///
    pub fn get_terrain_counts(&self) -> Census {
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

    ///
    /// gets a vector of interactable neighbours locations
    /// interactable is specified by closure.
    ///
    /// Examples of interactable are neighbours to move to or
    /// plant to
    ///
    pub fn get_interactable_neighbours_locations(
        &self,
        x: usize,
        y: usize,
        criteria: fn(ForestFeature) -> bool,
    ) -> Vec<(usize, usize)> {
        [-1, 0, 1]
            .iter()
            .cloned()
            .map(|delta_x| {
                [-1, 0, 1].iter().cloned().filter_map(move |delta_y| {
                    let new_x = delta_x + x as i32;
                    let new_y = delta_y + y as i32;

                    if (delta_x == 0 && delta_y == 0)
                        || new_x as usize >= self.size
                        || new_y as usize >= self.size
                        || new_x < 0
                        || new_y < 0
                    {
                        None
                    } else {
                        let new_x = new_x as usize;
                        let new_y = new_y as usize;

                        let neighbour = self.get(new_x, new_y);

                        if criteria(neighbour) {
                            Some((new_x, new_y))
                        } else {
                            None
                        }
                    }
                })
            })
            .flatten()
            .collect()
    }

    ///
    /// Takes a closure that filters for ceratin Foresfeature types
    /// applies the closure to every Forestfeature and
    /// returns a vector of all valid locations as tuple (x,y)
    ///
    pub fn get_locations(&self, criteria: fn(ForestFeature) -> bool) -> Vec<(usize, usize)> {
        self.layout
            .par_iter()
            .enumerate()
            .filter_map(|(i, x)| {
                if criteria(*x) {
                    Some(self.get_x_y(i))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn get_locations_with_info<T>(
        &self,
        criteria: fn(ForestFeature) -> Option<T>,
    ) -> Vec<FeatureLocation<T>>
    where
        T: Send,
    {
        self.layout
            .par_iter()
            .enumerate()
            .filter_map(|(i, x)| match criteria(*x) {
                Some(loc) => {
                    let (x, y) = self.get_x_y(i);
                    Some(FeatureLocation { x, y, feature: loc })
                }
                _ => None,
            })
            .collect()
    }

    ///
    /// changes a forest feature into another by slice of locations
    /// forest feature transformation is specified by
    /// closure
    ///
    pub fn transform_features(
        &mut self,
        new_locations: &[(usize, usize)],
        transformation: fn(ForestFeature) -> ForestFeature,
    ) {
        for position in new_locations {
            let (new_x, new_y) = position;
            let new_loc = self.get(*new_x, *new_y);
            self.assign_at(*new_x, *new_y, transformation(new_loc));
        }
    }

    ///
    /// Used to simulate movement in a forest
    /// take a FeatureLocation that stores stuff
    /// about the moving feature plus its location
    /// coridnates to new location
    /// and too closures to simulate movement one changes the orign
    /// the other the destionation T is any statistical information that the destination closure may return
    ///
    pub fn batched_move_to<T, U>(
        &mut self,
        moved_info: &U,
        move_form: fn(ForestFeature, usize, &U) -> ForestFeature,
        move_to: fn(ForestFeature, usize, &U) -> (ForestFeature, T),
    ) -> Vec<T>
    where
        U: Sync,
        T: Send,
    {
        let (new, results): (Vec<ForestFeature>, Vec<T>) = self
            .layout
            .par_iter()
            .enumerate()
            .map(|(i, old_loc)| (i, move_form(*old_loc, i, moved_info)))
            .map(|(i, new_loc)| move_to(new_loc, i, moved_info))
            .unzip();

        self.layout = new;

        //resul
        results
    }

    ///
    /// Calls the aging implementation on all
    /// forestfeatures in the forest
    ///
    pub fn age_features(&mut self) {
        self.layout = self.layout.par_iter().map(|x| x.age()).collect();
    }

    ///
    /// Changes the features to be initial state
    /// this will reset movement and lumber statistics
    ///
    pub fn reset_feature_state(&mut self) {
        self.layout.par_iter_mut().for_each(|x| match x {
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
        });
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

pub mod actions {

    use super::Forest;
    use crate::terrain::*;

    use rand::rngs::ThreadRng;
    use rand::Rng;
    use rayon::prelude::*;

    use std::cmp::Ordering;
    use std::collections::HashMap;
    use std::collections::HashSet;

    use std::{
        sync::Arc,
        sync::{
            mpsc::{channel, Sender},
            Mutex,
        },
    };

    use threadpool::ThreadPool;

    struct Hashes {
        trees: HashSet<usize>,
        before_lumbers: HashSet<usize>,
        lumbers: HashMap<usize, Woodcutter>,
        before_bears: HashSet<usize>,
        bears: HashMap<usize, BearInfo>,
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    enum ChangeEvent {
        SaplingPlanted,
        TreeCutDown(u32),
        LumberJackMauled(Woodcutter),
        SaplingPlantedLumberJackMauled(Woodcutter),
    }
    #[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
    struct Event {
        saplings_planted: u32,
        wood_cut: u32,
        lumberjacks_mauled: u32,
    }

    ///
    /// Parallel simulation
    ///
    pub fn simulate(size: usize, logging_channel: Sender<String>) {
        let mut rng = rand::thread_rng();
        let mut simulated_forest = Forest::new(size);

        let forest_pool = ThreadPool::new(3);

        let logging_pool = ThreadPool::new(6);

        let sync_time = Arc::new(Mutex::new((1, 1)));

        /* The main simulation */
        for year in 1..400 {
            let mut annual_wood_chop = 0;
            let mut annual_mualing = 0;
            let mut annual_sapling_plant = 0;
            for month in 1..=12 {
                //simulated_forest.age_features();
                simulated_forest.age_features();
                let mut monthly_changes = Vec::with_capacity(5);

                for move_phase in 0..5 {
                    monthly_changes.push(process_terrain(&mut simulated_forest, move_phase, &forest_pool));
                }

                simulated_forest.reset_feature_state();

                let monthly_changes = monthly_changes.into_par_iter().reduce(
                    || Event {
                        saplings_planted: 0,
                        wood_cut: 0,
                        lumberjacks_mauled: 0,
                    },
                    |a, b| Event {
                        saplings_planted: a.saplings_planted + b.saplings_planted,
                        wood_cut: a.wood_cut + b.wood_cut,
                        lumberjacks_mauled: a.lumberjacks_mauled + b.lumberjacks_mauled,
                    },
                );

                let monthly_log = logging_channel.clone();

                let forest_string = simulated_forest.to_string();

                let current_sync = sync_time.clone();

                logging_pool.execute(move || {

                    loop {
                        //println!("month mutex request");
                        let mut sync =  match current_sync.lock() {
                            Ok(a) => a,
                            e => {print!("{:?}",e); std::process::exit(0);} ,
                        };
                        let (sync_year,mut sync_month) = *sync;

                        if  sync_year == year && sync_month == month  {
                            monthly_log.send(forest_string).unwrap();
                            monthly_log.send(format!(
                                "month {} year {}, units of wood chopped this month: {},  lumberjacks_malued: {}, saplings planted: {},",
                                month,year,monthly_changes.wood_cut, monthly_changes.lumberjacks_mauled, monthly_changes.saplings_planted)
        
                            ).unwrap();
        
                            monthly_log.send(format!("{:-<1$}", "", size * 2)).unwrap();

                            //if 0 then  print the yearlog
                            sync_month = (sync_month + 1) %13;

                            //println!("{}",sync_month);


                            *sync = (sync_year,sync_month);

                           // println!("sending month");

                            return
                        }

                   
                }
                });

                annual_wood_chop += monthly_changes.wood_cut;
                annual_mualing += monthly_changes.lumberjacks_mauled;
                annual_sapling_plant += monthly_changes.saplings_planted;
            }

            let censare = populate(
                &mut simulated_forest,
                &mut rng,
                annual_wood_chop,
                annual_mualing,
            );

            let current_sync = sync_time.clone();
            let yearly_log = logging_channel.clone();
            logging_pool.execute( 
                move|| loop {
                    let mut sync =  current_sync.lock().unwrap();
                    let ( mut sync_year,mut sync_month) = *sync;
                   // println!("year mutex request {}, {}, {}",year,sync_year,sync_month);

                    if  sync_year == year && sync_month == 0  {

                        yearly_log.send(format!("yearly census {}", censare)).unwrap();

                        yearly_log.send(format!(
                            "year {}, wood chopped this year:{} ,lumberJacks mauled:{} ,saplings planted:{} ",
                            year, annual_wood_chop, annual_mualing, annual_sapling_plant
                        )).unwrap();

                        yearly_log.send(format!("{:_<1$}", "", size * 2)).unwrap();

                       sync_month = 1;

                       sync_year += 1;

                        *sync = (sync_year,sync_month);

                        //println!("sending year");
                        return;

                    }
                }

            )

        }
    }

    fn process_terrain(simulated_forest: &mut Forest, move_phase: u32, pool: &ThreadPool) -> Event {
        let changes = collect_movements(&simulated_forest, move_phase, pool);

        let moved_from_actions = |old_loc, i, movements: &Hashes| match old_loc {
            ForestFeature::LumberJack(l) => {
                if movements.before_lumbers.contains(&i) {
                    ForestFeature::Empty
                } else {
                    ForestFeature::LumberJack(l)
                }
            }
            ForestFeature::LumberSeedling(l, sap) => {
                if movements.before_lumbers.contains(&i) {
                    ForestFeature::Tree(FloraVariant::Sapling(sap))
                } else {
                    ForestFeature::LumberSeedling(l, sap)
                }
            }
            ForestFeature::Bear(b) => {
                if movements.before_bears.contains(&i) {
                    ForestFeature::Empty
                } else {
                    ForestFeature::Bear(b)
                }
            }
            ForestFeature::BearTree(b, tree) => {
                if movements.before_bears.contains(&i) {
                    match tree {
                        FloraVariant::Elder(l) => ForestFeature::Tree(FloraVariant::Elder(l)),
                        FloraVariant::Tree(l) => ForestFeature::Tree(FloraVariant::Tree(l)),
                        FloraVariant::Sapling(sap) => {
                            ForestFeature::Tree(FloraVariant::Sapling(sap))
                        }
                    }
                } else {
                    ForestFeature::BearTree(b, tree)
                }
            }
            l => l,
        };

        let moved_to_actions = |new_loc, i, movements: &Hashes| {
            if let Some(bear) = movements.bears.get(&i) {
                if let Some(lum) = movements.lumbers.get(&i) {
                    if movements.trees.contains(&i) {
                        //all three interact
                        match new_loc {
                            ForestFeature::Empty => (
                                ForestFeature::BearTree(
                                    BearInfo::moved(),
                                    FloraVariant::Sapling(Seedling {
                                        current_age: i as u32,
                                    }),
                                ),
                                Some(ChangeEvent::SaplingPlantedLumberJackMauled(*lum)),
                            ),
                            _ => panic!(" bear tree sap moved into undesriable location"),
                        }
                    } else {
                        // bear and lumberjack interact
                        match new_loc {
                            ForestFeature::Empty => (
                                ForestFeature::Bear(BearInfo::moved()),
                                Some(ChangeEvent::LumberJackMauled(*lum)),
                            ),
                            ForestFeature::Tree(l) => match l {
                                FloraVariant::Sapling(a) => (
                                    ForestFeature::BearTree(
                                        BearInfo::moved(),
                                        FloraVariant::Sapling(a),
                                    ),
                                    Some(ChangeEvent::LumberJackMauled(*lum)),
                                ),

                                FloraVariant::Tree(tree) => (
                                    ForestFeature::BearTree(
                                        BearInfo::moved(),
                                        FloraVariant::Tree(tree),
                                    ),
                                    Some(ChangeEvent::LumberJackMauled(*lum)),
                                ),

                                FloraVariant::Elder(tree) => (
                                    ForestFeature::BearTree(
                                        BearInfo::moved(),
                                        FloraVariant::Elder(tree),
                                    ),
                                    Some(ChangeEvent::LumberJackMauled(*lum)),
                                ),
                            },
                            _ => panic!("moved bear to invalid position lumber"),
                        }
                    }
                } else if movements.trees.contains(&i) {
                    // bear and tree interact
                    match new_loc {
                        ForestFeature::Empty => (
                            ForestFeature::BearTree(
                                *bear,
                                FloraVariant::Sapling(Seedling { current_age: 0 }),
                            ),
                            None,
                        ),
                        ForestFeature::LumberJack(lum) => (
                            ForestFeature::BearTree(
                                BearInfo::moved(),
                                FloraVariant::Sapling(Seedling { current_age: 0 }),
                            ),
                            Some(ChangeEvent::LumberJackMauled(lum)),
                        ),
                        _ => panic!("bear and sapling went into invalid"),
                    }
                } else {
                    // just the bear moves
                    match new_loc {
                        ForestFeature::Empty => (ForestFeature::Bear(*bear), None),
                        ForestFeature::LumberJack(lum) => (
                            ForestFeature::Bear(BearInfo::moved()),
                            Some(ChangeEvent::LumberJackMauled(lum)),
                        ),
                        ForestFeature::LumberSeedling(lum, s) => (
                            ForestFeature::BearTree(BearInfo::moved(), FloraVariant::Sapling(s)),
                            Some(ChangeEvent::LumberJackMauled(lum)),
                        ),
                        ForestFeature::Tree(l) => match l {
                            FloraVariant::Sapling(a) => (
                                ForestFeature::BearTree(*bear, FloraVariant::Sapling(a)),
                                None,
                            ),

                            FloraVariant::Tree(tree) => (
                                ForestFeature::BearTree(*bear, FloraVariant::Tree(tree)),
                                None,
                            ),

                            FloraVariant::Elder(tree) => (
                                ForestFeature::BearTree(*bear, FloraVariant::Elder(tree)),
                                None,
                            ),
                        },
                        _ => panic!("bear moved to invalid position {:?}", new_loc),
                    }
                }
            } else if let Some(lum) = movements.lumbers.get(&i) {
                if movements.trees.contains(&i) {
                    // lumbers and trees
                    match new_loc {
                        ForestFeature::Empty => (
                            ForestFeature::LumberSeedling(*lum, Seedling { current_age: 0 }),
                            Some(ChangeEvent::SaplingPlanted),
                        ),
                        _ => panic!("lumber jack sap moved to invalid area"),
                    }
                } else {
                    //lumber jack alone
                    match new_loc {
                        ForestFeature::Empty => (ForestFeature::LumberJack(*lum), None),
                        ForestFeature::Tree(l) => match l {
                            FloraVariant::Sapling(a) => {
                                (ForestFeature::LumberSeedling(*lum, a), None)
                            }

                            FloraVariant::Tree(_) => (
                                ForestFeature::LumberJack(Woodcutter {
                                    lumber_collected: lum.lumber_collected + 1,
                                    finished_moving: true,
                                }),
                                Some(ChangeEvent::TreeCutDown(1)),
                            ),

                            FloraVariant::Elder(_) => (
                                ForestFeature::LumberJack(Woodcutter {
                                    lumber_collected: lum.lumber_collected + 2,
                                    finished_moving: true,
                                }),
                                Some(ChangeEvent::TreeCutDown(2)),
                            ),
                        },
                        _ => panic!("invalid"),
                    }
                }
            } else if movements.trees.contains(&i) {
                //trees alone
                if let ForestFeature::Empty = new_loc {
                    (
                        ForestFeature::Tree(FloraVariant::Sapling(Seedling { current_age: 0 })),
                        Some(ChangeEvent::SaplingPlanted),
                    )
                } else {
                    panic!("planting in a non empty zone {:?}", new_loc);
                }
            } else {
                (new_loc, None)
            }
        };

        simulated_forest
            .batched_move_to(&changes, moved_from_actions, moved_to_actions)
            .par_iter()
            .filter_map(|x| {
                x.map(|current_evnet| match current_evnet {
                    ChangeEvent::SaplingPlanted => Event {
                        saplings_planted: 1,
                        wood_cut: 0,
                        lumberjacks_mauled: 0,
                    },
                    ChangeEvent::TreeCutDown(wood) => Event {
                        saplings_planted: 0,
                        wood_cut: wood,
                        lumberjacks_mauled: 0,
                    },
                    ChangeEvent::LumberJackMauled(lum) => Event {
                        saplings_planted: 0,
                        wood_cut: lum.lumber_collected,
                        lumberjacks_mauled: 1,
                    },
                    ChangeEvent::SaplingPlantedLumberJackMauled(lum) => Event {
                        saplings_planted: 1,
                        wood_cut: lum.lumber_collected,
                        lumberjacks_mauled: 1,
                    },
                })
            })
            .reduce(
                || Event {
                    saplings_planted: 0,
                    wood_cut: 0,
                    lumberjacks_mauled: 0,
                },
                |a, b| Event {
                    saplings_planted: a.saplings_planted + b.saplings_planted,
                    wood_cut: a.wood_cut + b.wood_cut,
                    lumberjacks_mauled: a.lumberjacks_mauled + b.lumberjacks_mauled,
                },
            )
    }

    fn collect_movements(simulated_forest: &Forest, move_phase: u32, pool: &ThreadPool) -> Hashes {
        let (tree_transmitter, tree_receiver) = channel();
        let (lumber_transmitter, lumber_receiver) = channel();
        let (bear_transmiter, bear_receiver) = channel();

        let shared_forest = Arc::new(simulated_forest.to_owned());

        let tree_forest = Arc::clone(&shared_forest);
        pool.execute(move || {
            let tree_transmitter = tree_transmitter;
            tree_transmitter
                .send(if move_phase == 0 {
                    process_spawning(&tree_forest, &mut rand::thread_rng())
                } else {
                    HashSet::new()
                })
                .unwrap();
        });

        let lumber_forest = Arc::clone(&shared_forest);
        pool.execute(move || {
            let lumber_transmitter = lumber_transmitter;
            lumber_transmitter
                .send(if move_phase < 3 {
                    process_lumberjacks(&lumber_forest, &mut rand::thread_rng())
                } else {
                    (HashSet::new(), HashMap::new())
                })
                .unwrap();
        });

        let bear_forest = Arc::clone(&shared_forest);

        pool.execute(move || {
            let bear_transmiter = bear_transmiter;
            bear_transmiter
                .send(process_bear(&bear_forest, &mut rand::thread_rng()))
                .unwrap();
        });

        //let (before_bears, after_bears) = process_bear(simulated_forest, &mut rand::thread_rng());

        let seeds = tree_receiver.recv().unwrap();
        let (before_lumbers, after_lumbers) = lumber_receiver.recv().unwrap();

        let (before_bears, after_bears) = bear_receiver.recv().unwrap();

        Hashes {
            trees: seeds,
            before_lumbers,
            lumbers: after_lumbers,
            before_bears,
            bears: after_bears,
        }
    }

    fn process_spawning(simulated_forest: &Forest, rng: &mut ThreadRng) -> HashSet<usize> {
        let mut changed_locations = HashSet::new();

        let mature_tress_criteria = |x| match x {
            ForestFeature::Tree(t) => match t {
                FloraVariant::Tree(_) => Some(MatureTree::Tree),
                FloraVariant::Elder(_) => Some(MatureTree::Elder),
                _ => None,
            },
            _ => None,
        };

        let mature_tress = simulated_forest.get_locations_with_info(mature_tress_criteria);

        /* main planting proceess happens here */
        for m in mature_tress {
            /* Random chance of planting */
            let gen = rng.gen_range(0, 10);

            match m.feature {
                MatureTree::Tree if gen == 0 => {
                    if let Some(i) = add_sap_to_forest(simulated_forest, rng, m, &changed_locations)
                    {
                        changed_locations.insert(i);
                    }
                    //simulated_forest.transform_feature((*new_site_x, *new_site_y), spawn_seedling);
                }
                MatureTree::Elder if gen == 0 || gen == 1 => {
                    if let Some(i) = add_sap_to_forest(simulated_forest, rng, m, &changed_locations)
                    {
                        changed_locations.insert(i);
                    }
                    //simulated_forest.transform_feature((*new_site_x, *new_site_y), spawn_seedling);
                }
                _ => continue,
            }
        }
        changed_locations
    }
    fn add_sap_to_forest(
        simulated_forest: &Forest,
        rng: &mut ThreadRng,
        m: FeatureLocation<MatureTree>,
        changed_locations: &HashSet<usize>,
    ) -> Option<usize> {
        let plantable_criteria = |x| match x {
            ForestFeature::Empty => true,
            _ => false,
        };
        let potential_locations =
            simulated_forest.get_interactable_neighbours_locations(m.x, m.y, plantable_criteria);

        let potential_locations = potential_locations
            .iter()
            .filter(|(x, y)| {
                changed_locations
                    .get(&simulated_forest.get_absolute_cordinate(*x, *y))
                    .is_none()
            })
            .collect::<Vec<&(usize, usize)>>();

        if potential_locations.is_empty() {
            return None;
        }

        let (new_site_x, new_site_y) = potential_locations
            .get(rng.gen_range(0, potential_locations.len()))
            .unwrap();

        Some(simulated_forest.get_absolute_cordinate(*new_site_x, *new_site_y))
    }

    fn process_lumberjacks(
        simulated_forest: &Forest,
        rng: &mut ThreadRng,
    ) -> (HashSet<usize>, HashMap<usize, Woodcutter>) {
        let mut moved_to_locations = HashMap::new();
        let mut moved_from_locations = HashSet::new();

        let movable_lumberjack_criteria = |x| match x {
            ForestFeature::LumberJack(l) if !l.finished_moving => Some(l),
            ForestFeature::LumberSeedling(l, _) if !l.finished_moving => Some(l),
            _ => None,
        };

        simulated_forest.get_locations_with_info::<Woodcutter>(movable_lumberjack_criteria);

        let lumber_jack_locations =
            simulated_forest.get_locations_with_info::<Woodcutter>(movable_lumberjack_criteria);

        let loacation_criteria = |x| match x {
            ForestFeature::Empty | ForestFeature::Tree(_) => true,
            _ => false,
        };

        for current_lumberjack in lumber_jack_locations {
            let new_destinations = simulated_forest.get_interactable_neighbours_locations(
                current_lumberjack.x,
                current_lumberjack.y,
                loacation_criteria,
            );

            let new_destinations = new_destinations
                .iter()
                .filter(|(x, y)| {
                    moved_to_locations
                        .get(&simulated_forest.get_absolute_cordinate(*x, *y))
                        .is_none()
                })
                .collect::<Vec<&(usize, usize)>>();

            if new_destinations.is_empty() {
                continue;
            }

            let (new_site_x, new_site_y) = new_destinations
                .get(rng.gen_range(0, new_destinations.len()))
                .unwrap();

            let new_absolute_loc =
                simulated_forest.get_absolute_cordinate(*new_site_x, *new_site_y);

            moved_to_locations.insert(new_absolute_loc, current_lumberjack.feature);

            let old_absolue_loc =
                simulated_forest.get_absolute_cordinate(current_lumberjack.x, current_lumberjack.y);

            moved_from_locations.insert(old_absolue_loc);
        }
        (moved_from_locations, moved_to_locations)
    }

    ///Movement of bears
    fn process_bear(
        simulated_forest: &Forest,
        rng: &mut ThreadRng,
    ) -> (HashSet<usize>, HashMap<usize, BearInfo>) {
        let mut moved_from_locations = HashSet::new();
        let mut moved_to_locations = HashMap::new();

        let movable_criteria = |x| match x {
            ForestFeature::Bear(b) if !b.finished_moving => Some(b),
            ForestFeature::BearTree(b, _) if !b.finished_moving => Some(b),
            _ => None,
        };

        let bear_locations = simulated_forest.get_locations_with_info(movable_criteria);

        let criteria = |x| match x {
            ForestFeature::Empty
            | ForestFeature::Tree(_)
            | ForestFeature::LumberJack(_)
            | ForestFeature::LumberSeedling(_, _) => true,
            _ => false,
        };

        for current_bear in bear_locations {
            let new_destinations = simulated_forest.get_interactable_neighbours_locations(
                current_bear.x,
                current_bear.y,
                criteria,
            );

            let new_destinations = new_destinations
                .iter()
                .filter(|(x, y)| {
                    moved_to_locations
                        .get(&simulated_forest.get_absolute_cordinate(*x, *y))
                        .is_none()
                })
                .collect::<Vec<&(usize, usize)>>();

            if new_destinations.is_empty() {
                continue;
            }
            let (new_site_x, new_site_y) = new_destinations
                .get(rng.gen_range(0, new_destinations.len()))
                .unwrap();

            let absolute_loc = simulated_forest.get_absolute_cordinate(*new_site_x, *new_site_y);

            moved_to_locations.insert(absolute_loc, current_bear.feature);

            let old_absolue_loc =
                simulated_forest.get_absolute_cordinate(current_bear.x, current_bear.y);

            moved_from_locations.insert(old_absolue_loc);
        }
        (moved_from_locations, moved_to_locations)
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
            bear_count: (censare.bear_count as i32 + delta_bears) as u32,
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
        /*Calculate lumber jack changes*/
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
                let spawnable_criteria = |x| match x {
                    ForestFeature::Empty => true,
                    ForestFeature::Tree(t) => match t {
                        FloraVariant::Sapling(_) => true,
                        _ => false,
                    },
                    _ => false,
                };

                let mut empty_locations = simulated_forest.get_locations(spawnable_criteria);

                let mut lumber_spawn_sites = Vec::with_capacity(delta_num_lumberjacks as usize);

                /* Collect randomly chosen spawn sites */
                for _ in 0..delta_num_lumberjacks {
                    let new_lumber_location =
                        empty_locations.remove(rng.gen_range(0, empty_locations.len()));
                    lumber_spawn_sites.push(new_lumber_location);
                }

                /*Instruct the forest on how to spawn the new lumberjacks*/
                let spawnable_transformation = |new_loc| match new_loc {
                    ForestFeature::Empty => ForestFeature::LumberJack(Woodcutter::new()),
                    ForestFeature::Tree(l) => match l {
                        FloraVariant::Sapling(a) => {
                            ForestFeature::LumberSeedling(Woodcutter::new(), a)
                        }
                        _ => panic!("spawning lumberJack invalid tree"),
                    },
                    _ => panic!("spawning lumberJack invalid location"),
                };
                simulated_forest.transform_features(&lumber_spawn_sites, spawnable_transformation);
            }
            Ordering::Less => {
                let criteria = |x| match x {
                    ForestFeature::LumberJack(_) => true,
                    ForestFeature::LumberSeedling(_, _) => true,
                    _ => false,
                };

                let mut cullable_lumberjacks = simulated_forest.get_locations(criteria);
                let culled_lumber_jack =
                    cullable_lumberjacks.remove(rng.gen_range(0, cullable_lumberjacks.len()));

                let remove_lumber_transform = |new_loc| match new_loc {
                    ForestFeature::LumberJack(_) => ForestFeature::Empty,
                    ForestFeature::LumberSeedling(_, s) => {
                        ForestFeature::Tree(FloraVariant::Sapling(s))
                    }
                    _ => panic!("spawning lumberJack invalid location"),
                };

                simulated_forest.transform_features(&[culled_lumber_jack], remove_lumber_transform);
            }
        }
        delta_num_lumberjacks
    }
    fn populate_bears(
        simulated_forest: &mut Forest,
        rng: &mut ThreadRng,
        maul_incidents: u32,
    ) -> i32 {
        match maul_incidents.cmp(&0) {
            Ordering::Equal | Ordering::Less => {
                /*where bears can spawn*/
                let spawnable_criteria = |x| match x {
                    ForestFeature::Empty => true,
                    ForestFeature::Tree(_) => true,
                    _ => false,
                };

                let mut bear_spawn_spots = simulated_forest.get_locations(spawnable_criteria);

                let new_bear_location =
                    bear_spawn_spots.remove(rng.gen_range(0, bear_spawn_spots.len()));

                /*Instructions on how to spawn bears*/
                let spawnable_transformation = |new_loc| match new_loc {
                    ForestFeature::Empty => ForestFeature::Bear(BearInfo::new()),
                    ForestFeature::Tree(l) => ForestFeature::BearTree(BearInfo::new(), l),
                    _ => panic!("spawning lumberJack invalid location"),
                };

                simulated_forest.transform_features(&[new_bear_location], spawnable_transformation);
                1
            }
            Ordering::Greater => {
                let criteria = |x| match x {
                    ForestFeature::Bear(_) => true,
                    ForestFeature::BearTree(_, _) => true,
                    _ => false,
                };
                let mut cullable_bears = simulated_forest.get_locations(criteria);
                let culled_bear = cullable_bears.remove(rng.gen_range(0, cullable_bears.len()));

                let remove_bear_transform = |new_loc| match new_loc {
                    ForestFeature::Bear(_) => ForestFeature::Empty,
                    ForestFeature::BearTree(_, t) => ForestFeature::Tree(t),
                    _ => panic!("spawning lumberJack invalid location"),
                };

                simulated_forest.transform_features(&[culled_bear], remove_bear_transform);
                -1
            }
        }
    }
}
