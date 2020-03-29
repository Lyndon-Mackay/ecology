extern crate rand;

mod terrain;

mod forests;

use forests::simple_forest::*;

pub fn simulate(size: usize) {
	actions::simulate(size);
}
