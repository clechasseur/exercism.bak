#pragma once

namespace dnd_character {

constexpr int base_hitpoints = 10;

int ability();
int modifier(int score);
    
struct Character
{
    int strength{ability()};
    int dexterity{ability()};
    int constitution{ability()};
    int intelligence{ability()};
    int wisdom{ability()};
    int charisma{ability()};
    int hitpoints{base_hitpoints + modifier(constitution)};
};

}  // namespace dnd_character
