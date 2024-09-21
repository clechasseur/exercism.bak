use std::collections::HashMap;
use std::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let mut freqs = HashMap::new();
    if input.is_empty() {
        return freqs;
    }

    // When input is too small, it's actually faster to do it sequentially.
    // Uncomment this when performing benchmarks if you want to beat the sequential implementation.
    // if input.len() <= 100 {
    //     return frequency_sequential(input);
    // }

    thread::scope(|scope| {
        let workers: Vec<_> = input.chunks(((input.len() as f64) / (worker_count as f64)).ceil() as usize)
            .map(|chunk| scope.spawn(|| frequency_sequential(chunk)))
            .collect();

        for worker in workers {
            for (c, n) in worker.join().unwrap() {
                *freqs.entry(c).or_default() += n;
            }
        }
    });

    freqs
}

fn frequency_sequential(input: &[&str]) -> HashMap<char, usize> {
    let mut h = HashMap::new();
    for &s in input {
        s.chars()
            .filter(|c| c.is_alphabetic())
            .for_each(|c| *h.entry(c.to_ascii_lowercase()).or_default() += 1);
    }
    h
}
