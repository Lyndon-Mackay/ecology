extern crate rand;

mod terrain;

mod forests;

//use forests::simple_forest::*;

use forests::parallel_forest::actions::simulate as parallel_forest_simulate;

pub fn simulate(size: usize) {
    parallel_forest_simulate(size);
}
