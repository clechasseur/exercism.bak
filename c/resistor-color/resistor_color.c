#include "resistor_color.h"

uint16_t color_code(resistor_band_t band)
{
    return band;
}

const resistor_band_t *colors()
{
    static const resistor_band_t COLORS[] = {
        BLACK,
        BROWN,
        RED,
        ORANGE,
        YELLOW,
        GREEN,
        BLUE,
        VIOLET,
        GREY,
        WHITE,
    };

    return COLORS;
}
