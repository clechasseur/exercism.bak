pub fn annotate(minefield: &[&str]) -> Vec<String> {
    minefield.iter()
        .enumerate()
        .map(|(y, &line)| {
            line.as_bytes().iter()
                .enumerate()
                .map(|(x, &c)| match c {
                    b'*' => '*',
                    b' ' => match mine_count(minefield, x, y) {
                        0 => ' ',
                        mc => char::from_digit(mc as u32, 10).unwrap(),
                    },
                    c => unreachable!("Invalid character found in minefield: {}", c as char),
                })
                .collect()
        })
        .collect()
}

const NEIGHBOUR_OFFSETS: &[(i32, i32)] = &[
    (-1, -1), (0, -1), (1, -1),
    (-1,  0),          (1,  0),
    (-1,  1), (0,  1), (1,  1),
];

fn mine_count(minefield: &[&str], x: usize, y: usize) -> usize {
    NEIGHBOUR_OFFSETS.iter()
        .map(|&(xd, yd)| (x as i32 + xd, y as i32 + yd))
        .filter(|&(_, ny)| (0..minefield.len() as i32).contains(&ny))
        .filter(|&(nx, ny)| (0..minefield[ny as usize].len() as i32).contains(&nx))
        .map(|(nx, ny)| minefield[ny as usize].as_bytes()[nx as usize])
        .filter(|&c| c == b'*')
        .count()
}
