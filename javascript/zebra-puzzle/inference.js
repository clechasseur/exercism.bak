import { popCount } from "./bitUtils";
import {
    BEVERAGE, BLUE, CHESTERFIELDS, CIGARETTE_BRAND,
    COFFEE,
    COLOR,
    DOG,
    ENGLISHMAN, FOX,
    GREEN, HORSE, IVORY, JAPANESE, KOOLS, LUCKY_STRIKE, MILK, NORWEGIAN,
    NUM_HOUSES, OLD_GOLD, ORANGE_JUICE,
    OWNER, PARLIAMENTS,
    PET,
    RED, SNAILS,
    SPANIARD, TEA,
    UKRAINIAN, YELLOW
} from "./domain";

// Attempts to add inferences to a puzzle.
// If the puzzle configuration was allowed by the statements, returns true and puzzle is updated.
// Otherwise, returns false and the value of puzzle is undefined.
export function addInferences(puzzle) {
    // Add inference for each statement, bailing out if one fails.
    for (const statement of STATEMENTS) {
        if (!statement.addInferences(puzzle)) {
            return false;
        }
    }

    // If we reach this point, inference is successful.
    return true;
}

// Inference for a puzzle statement that links two variable values
// together for one house. For example:
// "The Englishman lives in the red house."
class LinkStatement {
    constructor(offset1, value1, offset2, value2) {
        this.offset1 = offset1;
        this.value1 = value1;
        this.offset2 = offset2;
        this.value2 = value2;
    }

    addInferences(puzzle, reverse = false) {
        // Determine what variable we're looking for this time around.
        const offset1 = reverse ? this.offset2 : this.offset1;
        const value1 = reverse ? this.value2 : this.value1;
        const offset2 = reverse ? this.offset1 : this.offset2;
        const value2 = reverse ? this.value1 : this.value2;

        // Attempt to find a house whose variable 1 has the correct value.
        for (const house of puzzle.houses) {
            if (house.variables[offset1] === value1) {
                // If the house also has another value than value 2 set for variable 2, inference fails.
                const variable2 = house.variables[offset2];
                if (popCount(variable2) === 1 && variable2 !== value2) {
                    return false;
                }

                // Set house's variable 2 to value 2.
                house.variables[offset2] = value2;

                // Iterate the other houses.
                for (const otherHouse of puzzle.houses) {
                    if (otherHouse !== house) {
                        // If the other house has value 2 set for variable 2, inference fails.
                        const otherVariable2 = otherHouse.variables[offset2];
                        const otherVariableIsSet = popCount(otherVariable2) === 1;
                        if (otherVariableIsSet && otherVariable2 === value2) {
                            return false;
                        }

                        // Remove value 2 from possible values of other house's variable 2
                        // if its value is still undecided.
                        if (!otherVariableIsSet) {
                            otherHouse.variables[offset2] &= ~value2;
                        }
                    }
                }
            }
        }

        // If this is the first call, do the process in reverse.
        return reverse || this.addInferences(puzzle, true);
    }
}

// Inference for a puzzle statement that provides a fixed value
// for a variable in a given house. For example:
// "Milk is drunk in the middle house."
class PositionStatement {
    constructor(houseOffset, offset, value) {
        this.houseOffset = houseOffset;
        this.offset = offset;
        this.value = value;
    }

    addInferences(puzzle) {
        // Such statements actually set the variable of a house to a specific value.
        const house = puzzle.houses[this.houseOffset]
        const variable = house.variables[this.offset];

        // If the variable is already set to some other value, inference fails.
        if (popCount(variable) === 1 && variable !== this.value) {
            return false;
        }

        // Set the variable to its only valid value.
        house.variables[this.offset] = this.value;

        // Inference is a success.
        return true;
    }
}

// Inference for a puzzle statement that links two variable values
// for houses next to each other, regardless of order. For example:
// "The Norwegian lives next to the blue house."
class NeighbouringLinkStatement {
    constructor(offsetNeighbour1, valueNeighbour1, offsetNeighbour2, valueNeighbour2) {
        this.offsetNeighbour1 = offsetNeighbour1;
        this.valueNeighbour1 = valueNeighbour1;
        this.offsetNeighbour2 = offsetNeighbour2;
        this.valueNeighbour2 = valueNeighbour2;
    }

    addInferences(puzzle, reverse = false) {
        // Determine what variable we're looking for this time around.
        const offsetNeighbour1 = reverse ? this.offsetNeighbour2 : this.offsetNeighbour1;
        const valueNeighbour1 = reverse ? this.valueNeighbour2 : this.valueNeighbour1;
        const offsetNeighbour2 = reverse ? this.offsetNeighbour1 : this.offsetNeighbour2;
        const valueNeighbour2 = reverse ? this.valueNeighbour1 : this.valueNeighbour2;

        // Attempt to find a house whose variable 1 has the correct value.
        for (let houseIndex = 0; houseIndex < NUM_HOUSES; ++houseIndex) {
            const house = puzzle.houses[houseIndex];

            if (house.variables[offsetNeighbour1] === valueNeighbour1) {
                // This house has value 1 set for variable 1.

                // Check which other houses are neighbours of this house.
                const firstNeighbour = houseIndex > 0 ? puzzle.houses[houseIndex - 1] : puzzle.houses[1];
                const secondNeighbour = houseIndex < (NUM_HOUSES - 1) ? puzzle[houseIndex + 1] : undefined;

                if (!secondNeighbour) {
                    // House has only one neighbour.
                    // If that neighbour has a different value than value 2 for variable 2, inference fails.
                    const variable2 = firstNeighbour.variables[offsetNeighbour2];
                    const variableIsSet = popCount(variable2) === 1;
                    if (variableIsSet && variable2 !== valueNeighbour2) {
                        return false;
                    }

                    // Set the neighbour's variable 2 to value 2.
                    firstNeighbour.variables[offsetNeighbour2] = valueNeighbour2;
                } else {
                    // House has two neighbours.
                    const leftVariable2 = firstNeighbour.variables[offsetNeighbour2];
                    const rightVariable2 = secondNeighbour.variables[offsetNeighbour2];
                    const leftVariableIsSet = popCount(leftVariable2) === 1;
                    const rightVariableIsSet = popCount(rightVariable2) === 1;

                    if (leftVariableIsSet) {
                        if (leftVariable2 === valueNeighbour2) {
                            // Left neighbour has value 2 set for variable_ 2 already,
                            // so the right neighbour can't have it.
                            if (!rightVariableIsSet) {
                                secondNeighbour.variables[offsetNeighbour2] &= ~valueNeighbour2;
                            }
                        } else {
                            // Left neighbour has a different value than value 2 for variable 2,
                            // so the right neighbour must have value 2.
                            if (!rightVariableIsSet) {
                                secondNeighbour.variables[offsetNeighbour2] = valueNeighbour2;
                            } else if (rightVariable2 !== valueNeighbour2) {
                                // Right neighbour has a different value also; inference fails.
                                return false;
                            }
                        }
                    } else if (rightVariableIsSet) {
                        if (rightVariable2 === valueNeighbour2) {
                            // Right neighbour has value 2 for variable 2 already,
                            // so the left neighbour can't have it.
                            firstNeighbour.variables[offsetNeighbour2] &= ~valueNeighbour2;
                        } else {
                            // Right neighbour has a different value than value 2 for variable 2,
                            // so the left neighbour must have value 2.
                            firstNeighbour.variables[offsetNeighbour2] = valueNeighbour2;
                        }
                    }
                }

                // Iterate the other houses.
                for (const otherHouse of puzzle.houses) {
                    if (otherHouse !== house && otherHouse !== firstNeighbour && otherHouse !== secondNeighbour) {
                        // If the other house has value 2 set for variable 2, inference fails.
                        const otherVariable2 = otherHouse.variables[offsetNeighbour2];
                        const otherVariableIsSet = popCount(otherVariable2) === 1;
                        if (otherVariableIsSet && otherVariable2 === valueNeighbour2) {
                            return false;
                        }

                        // Remove value 2 from possible values of other house's variable 2
                        // if its value is still undecided.
                        if (!otherVariableIsSet) {
                            otherHouse.variables[offsetNeighbour2] &= ~valueNeighbour2;
                        }
                    }
                }
            }
        }

        // If this is the first call, do the process in reverse.
        return reverse || this.addInferences(puzzle, true);
    }
}

// Inference for a puzzle statement that links two variable values
// for houses next to each other, but in a specific order. There's only
// one statement like that:
// "The green house is immediately to the right of the ivory house."
class OrderedNeighbouringLinkStatement {
    constructor(leftOffset, leftValue, rightOffset, rightValue) {
        this.leftOffset = leftOffset;
        this.leftValue = leftValue;
        this.rightOffset = rightOffset;
        this.rightValue = rightValue;
    }

    addInferences(puzzle, reverse = false) {
        // Determine what variable we're looking for this time around.
        const firstOffset = reverse ? this.rightOffset : this.leftOffset;
        const firstValue = reverse ? this.rightValue : this.leftValue;
        const secondOffset = reverse ? this.leftOffset : this.rightOffset;
        const secondValue = reverse ? this.leftValue : this.rightValue;
        const houseOffset = reverse ? -1 : 1;

        // Attempt to find a house whose first variable has the correct value.
        for (let houseIndex = 0; houseIndex < NUM_HOUSES; ++houseIndex) {
            const house = puzzle.houses[houseIndex];

            if (house.variables[firstOffset] === firstValue) {
                // This house has the first value set for its first variable.
                // The other house (directly to its left or right) should have
                // the second value set for its second variable.
                const secondHouseIndex = houseIndex + houseOffset;

                // If there's no second house, inference fails.
                if (secondHouseIndex < 0 || secondHouseIndex >= NUM_HOUSES) {
                    return false;
                }

                // If the second house has a value different from the second value
                // for its second variable, inference fails.
                const secondHouse = puzzle.houses[secondHouseIndex];
                const secondVariable = secondHouse.variables[secondOffset];
                const secondVariableIsSet = popCount(secondVariable) === 1;
                if (secondVariableIsSet && secondVariable !== secondValue) {
                    return false;
                }

                // Set the second house's second variable to the second value.
                secondHouse.variables[secondOffset] = secondValue;

                // Iterate the other houses.
                for (const otherHouse of puzzle.houses) {
                    if (otherHouse !== secondHouse) {
                        // If the other house has the second value set for its second variable, inference fails.
                        const otherSecondVariable = otherHouse.variables[secondOffset];
                        const otherVariableIsSet = popCount(otherSecondVariable) === 1;
                        if (otherVariableIsSet && otherSecondVariable === secondValue) {
                            return false;
                        }

                        // Remove the second value from possible values of other house's second variable
                        // if its value is still undecided.
                        if (!otherVariableIsSet) {
                            otherHouse.variables[secondOffset] &= ~secondValue;
                        }
                    }
                }
            }
        }

        // If this is the first call, do the process in reverse.
        return reverse || this.addInferences(puzzle, true);
    }
}

const STATEMENTS = [
    // "There are five houses."
    // (No inference needed for this one)

    // "The Englishman lives in the red house."
    new LinkStatement(OWNER, ENGLISHMAN, COLOR, RED),

    // "The Spaniard owns the dog."
    new LinkStatement(OWNER, SPANIARD, PET, DOG),

    // "Coffee is drunk in the green house."
    new LinkStatement(BEVERAGE, COFFEE, COLOR, GREEN),

    // "The Ukrainian drinks tea."
    new LinkStatement(OWNER, UKRAINIAN, BEVERAGE, TEA),

    // "The green house is immediately to the right of the ivory house."
    new OrderedNeighbouringLinkStatement(COLOR, IVORY, COLOR, GREEN),

    // "The Old Gold smoker owns snails."
    new LinkStatement(CIGARETTE_BRAND, OLD_GOLD, PET, SNAILS),

    // "Kools are smoked in the yellow house."
    new LinkStatement(CIGARETTE_BRAND, KOOLS, COLOR, YELLOW),

    // "Milk is drunk in the middle house."
    new PositionStatement(2, BEVERAGE, MILK),

    // "The Norwegian lives in the first house."
    new PositionStatement(0, OWNER, NORWEGIAN),

    // "The man who smokes Chesterfields lives in the house next to the man with the fox."
    new NeighbouringLinkStatement(CIGARETTE_BRAND, CHESTERFIELDS, PET, FOX),

    // "Kools are smoked in the house next to the house where the horse is kept."
    new NeighbouringLinkStatement(CIGARETTE_BRAND, KOOLS, PET, HORSE),

    // "The Lucky Strike smoker drinks orange juice."
    new LinkStatement(CIGARETTE_BRAND, LUCKY_STRIKE, BEVERAGE, ORANGE_JUICE),

    // "The Japanese smokes Parliaments."
    new LinkStatement(OWNER, JAPANESE, CIGARETTE_BRAND, PARLIAMENTS),

    // "The Norwegian lives next to the blue house."
    new NeighbouringLinkStatement(OWNER, NORWEGIAN, COLOR, BLUE),
];
