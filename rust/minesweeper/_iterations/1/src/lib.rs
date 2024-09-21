use std::iter;

pub fn annotate(minefield: &[&str]) -> Vec<String> {
    minefield.iter()
        .enumerate()
        .map(|(y, &line)| {
            line.as_bytes().iter()
                .enumerate()
                .map(|(x, &c)| match c as char {
                    '*' => '*',
                    ' ' => match mine_count(minefield, x, y) {
                        0 => ' ',
                        mc => char::from_digit(mc as u32, 10).unwrap(),
                    },
                    c => unreachable!("Invalid character found in minefield: {}", c),
                })
                .collect()
        })
        .collect()
}

fn mine_count(minefield: &[&str], x: usize, y: usize) -> usize {
    let mut mines = 0usize;
    neighbours(x as i32, y as i32).for_each(|(nx, ny)| {
        if (0..minefield.len() as i32).contains(&ny) {
            let line = minefield[ny as usize];
            if (0..line.len() as i32).contains(&nx) && line.as_bytes()[nx as usize] == '*' as u8 {
                mines += 1;
            }
        }
    });
    mines
}

fn neighbours(x: i32, y: i32) -> impl Iterator<Item=(i32, i32)> {
    let mut xd = -1;
    let mut yd = -1;
    iter::from_fn(move || {
        if xd == 2 {
            xd = -1;
            yd += 1;
        }
        if xd == 0 && yd == 0 {
            xd += 1;
        }
        if yd <= 1 {
            let n = (x + xd, y + yd);
            xd += 1;
            Some(n)
        } else {
            None
        }
    })
}
