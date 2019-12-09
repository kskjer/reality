use reality_ultra::rom::Rom;
use std::{env, fs};

fn main() {
    let args: Vec<_> = env::args().collect();

    println!("args = {:?}", args);

    fs::write(
        &args[2],
        Rom::new(fs::read(&args[1]).unwrap()).unwrap().data(),
    )
    .unwrap();
}
