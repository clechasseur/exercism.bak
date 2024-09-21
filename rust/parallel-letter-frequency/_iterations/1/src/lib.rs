use std::collections::HashMap;
use std::sync::mpsc;
use std::thread;

pub fn frequency(input: &[&str], worker_count: usize) -> HashMap<char, usize> {
    let mut freqs = HashMap::new();
    if input.is_empty() {
        return freqs;
    }

    thread::scope(|scope| {
        let (tx, rx) = mpsc::channel();
        input.chunks(((input.len() as f64) / (worker_count as f64)).ceil() as usize)
            .for_each(|chunk| {
                let worker_tx = tx.clone();
                scope.spawn(move || {
                    let mut h: HashMap<_, usize> = HashMap::new();
                    for &s in chunk {
                        s.chars()
                            .filter(|c| c.is_alphabetic())
                            .flat_map(|c| c.to_lowercase())
                            .for_each(|lc| *h.entry(lc).or_default() += 1);
                    }
                    worker_tx.send(h).unwrap();
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
