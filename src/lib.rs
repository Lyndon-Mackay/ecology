extern crate rand;

mod terrain;

mod forests;

//use forests::simple_forest::*;

use forests::parallel_forest::actions::simulate as parallel_forest_simulate;
use std::{sync::mpsc::channel, thread};

pub fn simulate(size: usize) {
    let (tr, rx) = channel();

    let mut threads = Vec::with_capacity(2);

    threads.push(thread::spawn(move || parallel_forest_simulate(size, tr)));

    threads.push(thread::spawn(move || loop {
        let log = rx.recv();

        match log {
            Ok(v) => {
                println!("{}", v);
            }
            Err(_) => {
                return;
            }
        }
    }));

    for t in threads {
        t.join().unwrap();
    }
}
