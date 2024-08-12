module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven actualMinutesInOven =
    expectedMinutesInOven - actualMinutesInOven

let preparationTimeInMinutes layers =
    layers * 2

let elapsedTimeInMinutes layers actualMinutesInOven =
    preparationTimeInMinutes layers + actualMinutesInOven
