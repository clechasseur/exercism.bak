pub fn raindrops(n: u32) -> String {
    match sounds(n) {
        s if s.is_empty() => n.to_string(),
        s => s,
    }
}

const SOUNDS: [(u32, &str); 3] = [(3, "Pling"), (5, "Plang"), (7, "Plong")];

fn sounds(n: u32) -> String {
    SOUNDS
        .iter()
        .filter_map(|&(factor, sound)| (n % factor == 0).then_some(sound))
        .collect()
}
