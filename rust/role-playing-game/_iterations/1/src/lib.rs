pub struct Player {
    pub health: u32,
    pub mana: Option<u32>,
    pub level: u32,
}

impl Player {
    pub fn revive(&self) -> Option<Player> {
        if self.health == 0 {
            Some(Player {
                health: 100,
                mana: match self.mana {
                    Some(_) => Some(100),
                    None => None
                },
                level: self.level
            })
        } else {
            None
        }
    }

    pub fn cast_spell(&mut self, mana_cost: u32) -> u32 {
        match self.mana {
            Some(cur_mana) => if cur_mana >= mana_cost {
                self.mana = Some(cur_mana - mana_cost);
                mana_cost * 2
            } else {
                0
            },
            None => {
                self.health = if self.health > mana_cost {
                    self.health - mana_cost
                } else {
                    0
                };
                0
            }
        }
    }
}
