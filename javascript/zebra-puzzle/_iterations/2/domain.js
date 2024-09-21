// Number of houses in the puzzle.
export const NUM_HOUSES = 5;

// Number of variables for each house.
export const NUM_VARIABLES = 5;

// Offset of each house variable.
export const OWNER           = 0;
export const COLOR           = 1;
export const BEVERAGE        = 2;
export const PET             = 3;
export const CIGARETTE_BRAND = 4;

// Sentinel value representing an invalid configuration
export const INVALID_VALUE = 0;

// Nationalities of house occupants
export const ENGLISHMAN = 0b00001;
export const SPANIARD   = 0b00010;
export const UKRAINIAN  = 0b00100;
export const NORWEGIAN  = 0b01000;
export const JAPANESE   = 0b10000;

// House colors
export const RED    = 0b00001;
export const GREEN  = 0b00010;
export const IVORY  = 0b00100;
export const YELLOW = 0b01000;
export const BLUE   = 0b10000;

// Beverages
export const COFFEE       = 0b00001;
export const TEA          = 0b00010;
export const MILK         = 0b00100;
export const ORANGE_JUICE = 0b01000;
export const WATER        = 0b10000;

// Pets
export const DOG    = 0b00001;
export const SNAILS = 0b00010;
export const FOX    = 0b00100;
export const HORSE  = 0b01000;
export const ZEBRA  = 0b10000;

// Cigarette brands
export const OLD_GOLD      = 0b00001;
export const KOOLS         = 0b00010;
export const CHESTERFIELDS = 0b00100;
export const LUCKY_STRIKE  = 0b01000;
export const PARLIAMENTS   = 0b10000;

// Sentinel value representing all values combined.
// Used to initialize a full bitfield of possible values.
export const ALL_POSSIBILITIES = 0b11111;
