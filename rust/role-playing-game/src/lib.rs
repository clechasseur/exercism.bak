pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        match self.health {
            0 => Some(Player {
                health: 100,
                mana: match self.mana {
                    Some(_) => Some(100),
                    None => None
                },
                ..*self
            }),
            _ => None
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(cur_mana) if cur_mana < mana_cost => 0,
            Some(cur_mana) => {
                self.mana.replace(cur_mana - mana_cost);
                mana_cost * 2
            },
            None => {
                self.health = match self.health {
                    h if h < mana_cost => 0,
                    h => h - mana_cost
                };
                0
            }
        }
    }
}
