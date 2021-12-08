use std::{fs::File, io::Read, time::Instant, iter::Inspect};

fn main() {
    let mut file = File::open("input").unwrap();
    let mut string = String::new();
    file.read_to_string(&mut string).unwrap();
    let line = string.lines().next().unwrap();
    let fishes = line.split(',').map(str::parse::<usize>).map(Result::unwrap).collect::<Vec<_>>();

    let start = Instant::now();
    println!("{}", run(&fishes, 80));
    println!("{}", run(&fishes, 256));
    let elapsed = Instant::now() - start;
    println!("Took {} microseconds", elapsed.as_micros())
}

fn other_run(fishes: &Vec<usize>, days: usize) -> u64 {
    let mut age_counts = [0u64;9];
    for fish in fishes { age_counts[*fish] += 1};
}

fn run(fishes: &Vec<usize>, days: usize) -> u64 {
    let mut age_counts = [0u64;9];
    for fish in fishes { age_counts[*fish] += 1}
    for _ in 0..days {
        age_counts.rotate_left(1);
        age_counts[6] += age_counts[8];
    }
    age_counts.iter().sum()
}
