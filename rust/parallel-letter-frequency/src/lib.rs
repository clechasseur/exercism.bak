use std::collections::HashMap;
use std::sync::mpsc;
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
        let (tx, rx) = mpsc::channel();
        input.chunks(((input.len() as f64) / (worker_count as f64)).ceil() as usize)
            .for_each(|chunk| {
                let worker_tx = tx.clone();
                scope.spawn(move || {
                    worker_tx.send(frequency_sequential(chunk)).unwrap();
                });
            });

        drop(tx);
        for wh in rx {
            for (c, n) in wh {
                *freqs.entry(c).or_default() += n;
            }
        }
    });

    freqs
}

fn frequency_sequential(input: &[&str]) -> HashMap<char, usize> {
    let mut h = HashMap::new();
    for &s in input {
        s.to_lowercase().chars()
            .filter(|c| c.is_alphabetic())
            .for_each(|c| *h.entry(c).or_default() += 1);
    }
    h
}
