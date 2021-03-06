use crate::terrain::*;
use std::fmt;

use rand::Rng;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Forest {
    layout: Vec<ForestFeature>,
    size: usize,
}

impl Forest {
    ///
    /// Generates a forest of specified size
    /// the height and width are always the same
    /// size
    ///
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
    /// Takes a closure that filters for ceratin Foresfeature types
    /// applies the closure to every Forestfeature and
    /// returns a vector of all valid locations as tuple (x,y)
    ///
    pub fn get_locations(&self, criteria: fn(ForestFeature) -> bool) -> Vec<(usize, usize)> {
        self.layout
            .iter()
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
    ///
    /// Takes a closure that filters for ceratin Foresfeature types
    /// with an option return value<T>
    /// applies the closure to every Forestfeature and
    /// returns a vector of all valid locations as vector of Feature locations<T>
    ///
    ///
    pub fn get_locations_with_info<T>(
        &self,
        criteria: fn(ForestFeature) -> Option<T>,
    ) -> Vec<FeatureLocation<T>> {
        self.layout
            .iter()
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
    /// Calls the aging implementation on all
    /// forestfeatures in the forest
    ///
    pub fn age_features(&mut self) {
        self.layout = self.layout.iter().map(|x| x.age()).collect();
    }

    /// sums the amount of wood chopped by the lumber jacks
    pub fn amount_of_wood_chopped(&self) -> u32 {
        self.layout
            .iter()
            .filter_map(|x| match x {
                ForestFeature::LumberJack(l) => Some(l.lumber_collected),
                ForestFeature::LumberSeedling(l, _) => Some(l.lumber_collected),
                _ => None,
            })
            .sum()
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

                if criteria(neighbour) {
                    returned_vec.push((new_x, new_y))
                }
            }
        }
        returned_vec
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
    /// changes a forest feature into another
    /// forest feature transformation is specified by
    /// closure
    ///
    pub fn transform_feature(
        &mut self,
        new_locations: (usize, usize),
        transformation: fn(ForestFeature) -> ForestFeature,
    ) {
        let (new_x, new_y) = new_locations;
        let new_loc = self.get(new_x, new_y);
        self.assign_at(new_x, new_y, transformation(new_loc));
    }
    ///
    /// Used to simulate movement in a forest
    /// take a FeatureLocation that stores stuff
    /// about the moving feature plus its location
    /// coridnates to new location
    /// and too closures to simulate movement one changes the orign
    /// the other the destionation T is any statistical information that the destination closure may return
    ///
    pub fn move_to<T, U>(
        &mut self,
        migrator: FeatureLocation<U>,
        new_loc: (usize, usize),
        move_form: fn(ForestFeature) -> ForestFeature,
        move_to: fn(ForestFeature, U) -> (ForestFeature, T),
    ) -> T {
        let old_loc = self.get(migrator.x, migrator.y);
        self.assign_at(migrator.x, migrator.y, move_form(old_loc));

        let (new_x, new_y) = new_loc;
        let new_loc = self.get(new_x, new_y);
        let (new_feature, resul) = move_to(new_loc, migrator.feature);
        self.assign_at(new_x, new_y, new_feature);

        resul
    }

    ///
    /// Changes the features to be initial state
    /// this will reset movement and lumber statistics
    ///
    pub fn reset_feature_state(&mut self) {
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

pub mod actions {

    use super::Forest as SimpleForest;

    use crate::terrain::*;
    use rand::rngs::ThreadRng;
    use rand::Rng;
    use std::cmp::Ordering;

    ///
    /// single hread simulate
    ///
    pub fn simulate(size: usize) {
        let mut rng = rand::thread_rng();
        let mut simulated_forest = SimpleForest::new(size);
        /* The main simulation */
        for year in 1..400 {
            let mut annual_wood_chop = 0;
            let mut annual_mualing = 0;
            let mut annual_sapling_plant = 0;
            for month in 1..=12 {
                simulated_forest.age_features();
                let saplings_planted_this_month = process_spawning(&mut simulated_forest, &mut rng);
                let mut mauled_lumberjacks = Vec::with_capacity(size * size);
                /* Movement of lumebrjacks and bear */
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
                /* reset statistics and movment */
                simulated_forest.reset_feature_state();
                /* End of month printing */
                println!("{}", simulated_forest);
                println!(
					"month {} year {}, units of wood chopped this month: {},  lumberjacks_malued: {}, saplings planted: {},",
					month,year,wood_chopped_this_month, mauled_lumberjacks_this_month, saplings_planted_this_month
				);
                println!("{:-<1$}", "", size * 2);
            }
            /* Enf of year printing and statsitics */
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
            /* Terminate no forest to simulate */
            if censare.tree_count == 0 {
                println!("The once great forest has disappeared");
                std::process::exit(0);
            }
        }
    }

    //
    /// Process the spawning logic of tress
    /// this will plant new saplings
    ///
    fn process_spawning(simulated_forest: &mut SimpleForest, rng: &mut ThreadRng) -> u32 {
        /* Describe what mature tress are to the forest */
        let mature_tress_criteria = |x| match x {
            ForestFeature::Tree(t) => match t {
                FloraVariant::Tree(_) => Some(MatureTree::Tree),
                FloraVariant::Elder(_) => Some(MatureTree::Elder),
                _ => None,
            },
            _ => None,
        };

        let mature_tress = simulated_forest.get_locations_with_info(mature_tress_criteria);

        let mut num_saplings_planted = 0;

        let plantable_criteria = |x| match x {
            ForestFeature::Empty => true,
            _ => false,
        };

        let spawn_seedling = |new_loc| {
            if let ForestFeature::Empty = new_loc {
                ForestFeature::Tree(FloraVariant::Sapling(Seedling { current_age: 0 }))
            } else {
                panic!("planting in a non empty zone {:?}", new_loc);
            }
        };

        /* main planting proceess happens here */
        for m in mature_tress {
            /* Random chance of planting */
            let gen = rng.gen_range(0, 10);
            match m.feature {
                MatureTree::Tree if gen == 0 => {
                    let potential_locations = simulated_forest
                        .get_interactable_neighbours_locations(m.x, m.y, plantable_criteria);
                    if potential_locations.is_empty() {
                        continue;
                    }
                    let (new_site_x, new_site_y) = potential_locations
                        .get(rng.gen_range(0, potential_locations.len()))
                        .unwrap();
                    simulated_forest.transform_feature((*new_site_x, *new_site_y), spawn_seedling);
                    num_saplings_planted += 1;
                }
                MatureTree::Elder if gen == 0 || gen == 1 => {
                    let potential_locations = simulated_forest
                        .get_interactable_neighbours_locations(m.x, m.y, plantable_criteria);

                    if potential_locations.is_empty() {
                        continue;
                    }

                    let (new_site_x, new_site_y) = potential_locations
                        .get(rng.gen_range(0, potential_locations.len()))
                        .unwrap();
                    simulated_forest.transform_feature((*new_site_x, *new_site_y), spawn_seedling);
                    num_saplings_planted += 1;
                }
                _ => continue,
            }
        }
        num_saplings_planted
    }
    /// Movement of lumberjacks handled here
    fn process_lumberjacks(simulated_forest: &mut SimpleForest, rng: &mut ThreadRng) {
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

            if new_destinations.is_empty() {
                continue;
            }

            let (new_x, new_y) = new_destinations
                .get(rng.gen_range(0, new_destinations.len()))
                .unwrap();

            let move_from_actions = |old_loc| match old_loc {
                ForestFeature::LumberJack(_) => ForestFeature::Empty,
                ForestFeature::LumberSeedling(_, sap) => {
                    ForestFeature::Tree(FloraVariant::Sapling(sap))
                }

                old_place => panic!("lumberjack disappered {:?}", old_place),
            };
            let move_to_actions = |new_loc, migrator| {
                let res = match new_loc {
                    ForestFeature::Empty => ForestFeature::LumberJack(migrator),
                    ForestFeature::Tree(l) => match l {
                        FloraVariant::Sapling(a) => ForestFeature::LumberSeedling(migrator, a),

                        FloraVariant::Tree(_) => ForestFeature::LumberJack(Woodcutter {
                            lumber_collected: migrator.lumber_collected + 1,
                            finished_moving: true,
                        }),

                        FloraVariant::Elder(_) => ForestFeature::LumberJack(Woodcutter {
                            lumber_collected: migrator.lumber_collected + 2,
                            finished_moving: true,
                        }),
                    },
                    _ => panic!("moved lumber to invalid position"),
                };
                (res, ())
            };

            simulated_forest.move_to(
                current_lumberjack,
                (*new_x, *new_y),
                move_from_actions,
                move_to_actions,
            );
        }
    }
    ///Movement of bears
    fn process_bear(simulated_forest: &mut SimpleForest, rng: &mut ThreadRng) -> Vec<Woodcutter> {
        let movable_criteria = |x| match x {
            ForestFeature::Bear(b) if !b.finished_moving => Some(b),
            ForestFeature::BearTree(b, _) if !b.finished_moving => Some(b),
            _ => None,
        };

        let bear_locations = simulated_forest.get_locations_with_info(movable_criteria);

        let mut maulings = Vec::with_capacity(bear_locations.len());

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
            if new_destinations.is_empty() {
                continue;
            }
            let (new_x, new_y) = new_destinations
                .get(rng.gen_range(0, new_destinations.len()))
                .unwrap();

            let move_from_actions = |old_loc| match old_loc {
                ForestFeature::Bear(_) => ForestFeature::Empty,
                ForestFeature::BearTree(_, tree) => match tree {
                    FloraVariant::Elder(l) => ForestFeature::Tree(FloraVariant::Elder(l)),
                    FloraVariant::Tree(l) => ForestFeature::Tree(FloraVariant::Tree(l)),
                    FloraVariant::Sapling(sap) => ForestFeature::Tree(FloraVariant::Sapling(sap)),
                },

                old_place => panic!("bear disappered {:?}", old_place),
            };

            let move_to_actions = |new_loc, bear| match new_loc {
                ForestFeature::Empty => (ForestFeature::Bear(bear), None),

                ForestFeature::Tree(l) => match l {
                    FloraVariant::Sapling(a) => (
                        ForestFeature::BearTree(bear, FloraVariant::Sapling(a)),
                        None,
                    ),

                    FloraVariant::Tree(tree) => (
                        ForestFeature::BearTree(bear, FloraVariant::Tree(tree)),
                        None,
                    ),

                    FloraVariant::Elder(tree) => (
                        ForestFeature::BearTree(bear, FloraVariant::Elder(tree)),
                        None,
                    ),
                },
                ForestFeature::LumberJack(l) => (
                    ForestFeature::Bear(BearInfo {
                        finished_moving: true,
                    }),
                    Some(l),
                ),
                ForestFeature::LumberSeedling(l, sap) => (
                    ForestFeature::BearTree(
                        BearInfo {
                            finished_moving: true,
                        },
                        FloraVariant::Sapling(sap),
                    ),
                    Some(l),
                ),

                _ => panic!("moved bear to invalid position"),
            };

            let result = simulated_forest.move_to(
                current_bear,
                (*new_x, *new_y),
                move_from_actions,
                move_to_actions,
            );
            maulings.push(result);
        }
        maulings.into_iter().filter_map(|x| x).collect()
    }

    fn populate(
        simulated_forest: &mut SimpleForest,
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
        simulated_forest: &mut SimpleForest,
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
                for _x in 0..delta_num_lumberjacks {
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

                simulated_forest.transform_feature(culled_lumber_jack, remove_lumber_transform);
            }
        }
        delta_num_lumberjacks
    }
    fn populate_bears(
        simulated_forest: &mut SimpleForest,
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

                simulated_forest.transform_feature(new_bear_location, spawnable_transformation);
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

                simulated_forest.transform_feature(culled_bear, remove_bear_transform);
                -1
            }
        }
    }
}
