#include "resistor_color_duo.h"

static inline uint16_t band_value(resistor_band_t band)
{
    return band;
}

uint16_t color_code(const resistor_band_t *bands)
{
    return band_value(bands[0]) * 10 + band_value(bands[1]);
}
