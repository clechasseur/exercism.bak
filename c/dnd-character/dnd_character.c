#include "dnd_character.h"

#include <stdlib.h>

static inline int roll_die(void)
{
    return (rand() % 6) + 1;
}

int ability(void)
{
    // Roll the first die.
    int score = roll_die();
    int lowest = score;

    // Roll two more dice, keeping track of smallest.
    for (int i = 0; i < 2; ++i) {
        const int die = roll_die();
        if (die < lowest) {
            lowest = die;
        }
        score += die;
    }

    // Roll a final die. If it's higher than the lowest dice, switch.
    const int final_die = roll_die();
    if (final_die > lowest) {
        score -= lowest;
        score = final_die;
    }

    return score;
}

int modifier(int score)
{
    return (score / 2) - 5;
}

dnd_character_t make_dnd_character(void)
{
    dnd_character_t character = {
        .strength     = ability(),
        .dexterity    = ability(),
        .constitution = ability(),
        .intelligence = ability(),
        .wisdom       = ability(),
        .charisma     = ability(),
        .hitpoints    = 10
    };
    character.hitpoints += modifier(character.constitution);

    return character;
}
