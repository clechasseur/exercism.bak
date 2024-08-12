const ACTIONS: &[&str] = &["wink", "double blink", "close your eyes", "jump"];

pub fn actions(n: u8) -> Vec<&'static str> {
    let mut commands = Vec::new();
    for i in 0..ACTIONS.len() {
        if n & (1 << i) != 0 {
            commands.push(ACTIONS[i]);
        }
    }
    if n & 0b10000 != 0 {
        commands.reverse();
    }
    commands
}

