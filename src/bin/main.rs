use ecology::simulate;

use clap::{App, Arg, SubCommand};
use std::env;
fn main() {
    let matches = App::new("My Super Program")
        .version("1.0")
        .author("lyndon Mackay")
        .about("SImulated Forest")
        .arg(
            Arg::with_name("Size")
                .help("The size of the forest an integer greater then 0")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("multi")
                .short("m")
                .long("multi")
                .help("Run in multi Threaded mode"),
        )
        .get_matches();

    let size = matches
        .value_of("Size")
        .unwrap()
        .parse()
        .expect(" SIze should be an integer greater then 0");

    // simulate(size);

    let muli_thread = matches.is_present("multi");

    println!("{}", muli_thread);

    simulate(size, muli_thread);
}
