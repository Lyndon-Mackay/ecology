use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Seedling {
	pub current_age: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FloraVariant {
	Sapling(Seedling),
	Tree(u32),
	Elder(u32),
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct Woodcutter {
	pub lumber_collected: u32,
	pub finished_moving: bool,
}

#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub struct BearInfo {
	pub finished_moving: bool,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ForestFeature {
	Empty,
	Bear(BearInfo),
	LumberJack(Woodcutter),
	Tree(FloraVariant),
	LumberSeedling(Woodcutter, Seedling),
	BearTree(BearInfo, FloraVariant),
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum MatureTree {
	Tree,
	Elder,
}
/// stores information and location about
/// a feature substruce
///
pub struct FeatureLocation<T> {
	pub feature: T,
	pub x: usize,
	pub y: usize,
}

pub struct Census {
	pub tree_count: u32,
	pub bear_count: u32,
	pub lumberjack_count: u32,
}

///
/// Used for things that have a concept of age in this sinulation
/// This allows the being to age itself
///
pub trait Growing {
	type Item;
	fn age(&self) -> Self::Item;
}

impl Growing for FloraVariant {
	type Item = FloraVariant;
	fn age(&self) -> Self::Item {
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
impl Growing for ForestFeature {
	type Item = ForestFeature;
	fn age(&self) -> Self::Item {
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

impl Woodcutter {
	pub fn new() -> Self {
		Woodcutter {
			lumber_collected: 0,
			finished_moving: false,
		}
	}
}
impl BearInfo {
	pub fn new() -> Self {
		BearInfo {
			finished_moving: false,
		}
	}
	pub fn moved() -> Self {
		BearInfo {
			finished_moving: false,
		}
	}
}
impl fmt::Display for Census {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		writeln!(
			f,
			"Trees:{}, Bears:{}, Lumberjacks:{}",
			self.tree_count, self.bear_count, self.lumberjack_count
		)?;

		Ok(())
	}
}
