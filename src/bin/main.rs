use ecology::simulate;

use std::env;
fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        panic!("Should have one argument the size of the map");
    }
    let size = &args[1];

    let size = size.parse().expect("positive number expected");
    // simulate(size);
    simulate(size);
}
