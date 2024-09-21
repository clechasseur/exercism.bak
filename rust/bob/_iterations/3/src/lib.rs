pub fn reply(message: &str) -> &str {
    let message = message.trim();

    match (message.is_empty(), message.ends_with('?'), is_yelling(message)) {
        (true, false, false) => "Fine. Be that way!",
        (true, _, _) => unreachable!(),
        (false, true, false) => "Sure.",
        (false, false, true) => "Whoa, chill out!",
        (false, true, true) => "Calm down, I know what I'm doing!",
        (false, false, false) => "Whatever.",
    }
}

fn is_yelling(message: &str) -> bool {
    message
        .chars()
        .try_fold(false, |yelling, c| {
            (!c.is_lowercase())
                .then(|| yelling || c.is_uppercase())
                .ok_or(())
        })
        .unwrap_or(false)
}
