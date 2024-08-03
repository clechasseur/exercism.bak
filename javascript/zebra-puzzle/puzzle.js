import { firstBit, popCount } from "./bitUtils";
import { ALL_POSSIBILITIES, INVALID_VALUE, NUM_VARIABLES, OWNER } from "./domain";
import { addInferences } from "./inference";

// Storage for the configuration of one house in the puzzle.
// Possible values for each variable are stored as bitfields.
// When only one bit remains, it's the value associated with the variable.
class House {
    // Constructs a new house configuration, with all variables set to all possibilities.
    constructor() {
        this.variables = [
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
        ];
    }

    // Assigns the values of the given house's variables to this house.
    assign(house) {
        house.variables.forEach((variable, offset) => this.variables[offset] = variable);
    }
}

// Storage for a puzzle configuration. Contains one House per house.
export class Puzzle {
    // Constructs an initial puzzle configuration.
    // Each house has all its variables set to all possibilities.
    constructor() {
        this.houses = [new House(), new House(), new House(), new House(), new House()];
    }

    // Checks if the puzzle configuration is valid.
    // The configuration is valid if no two houses have the same value for a given variable
    // and if no variable is set to an invalid value.
    get valid() {
        const seenValues = [0, 0, 0, 0, 0];

        // Iterate each house to check their values.
        return this.houses.every((house) => {
            // Iterate each variable for this house.
            return house.variables.every((variable, offset) => {
                // If the variable has an invalid value, the puzzle configuration is invalid.
                if (variable === INVALID_VALUE) {
                    return false;
                }

                // If the variable has a single bit set (meaning it has a definitive value,
                // it's not just a bitfield of possibilities) and we've seen that value before,
                // the puzzle configuration is invalid.
                if (popCount(variable) === 1) {
                    if ((seenValues[offset] & variable) !== 0) {
                        return false;
                    }

                    // Note the value of this variable as "seen".
                    seenValues[offset] |= variable;
                }

                // If we made it here, the variable is valid.
                return true;
            });
        });
    }

    // Checks if the given puzzle configuration is complete.
    // The configuration is complete if all variables have different values for each house.
    get complete() {
        const remainingValues = [
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
            ALL_POSSIBILITIES,
        ];

        // Iterate each house to check their values.
        return this.houses.every((house) => {
            // Iterate each variable for this house.
            return house.variables.every((variable, offset) => {
                // If the variable does not have a single bit set (meaning it still
                // has multiple possibilities, or it has an invalid value), or if the
                // value of this variable was already used, then puzzle configuration
                // is not complete.
                if (popCount(variable) !== 1 || (remainingValues[offset] & variable) === 0) {
                    return false;
                }

                // Remove this variable's value from the possibilities.
                remainingValues[offset] &= ~variable;

                // If we made it here, the variable is valid.
                return true;
            });
        });
    }

    // Assigns the values of all houses' variables of the given puzzle to this puzzle.
    assign(puzzle) {
        puzzle.houses.forEach((house, i) => this.houses[i].assign(house));
    }

    // Solves the puzzle and leaves it in the valid configuration.
    solve() {
        if (!this.backtrackingSolve()) {
            throw new Error("Could not solve puzzle");
        }
    }

    // Returns the owner of the house matching the given variable's value.
    ownerMatching(variableOffset, variableValue) {
        const house = this.houses.find((h) => h.variables[variableOffset] === variableValue);
        return house.variables[OWNER];
    }

    // Attempts to solve the puzzle.
    // If we could solve the puzzle, returns true and puzzle contains the solution.
    // Otherwise, returns false and puzzle is unmodified.
    backtrackingSolve() {
        // The algorithm used to solve the puzzle has been inspired by this post:
        // https://www.baeldung.com/cs/csp

        // If this puzzle configuration is complete, return it.
        if (this.complete) {
            return true;
        }

        // Select a variable which has not been assigned yet.
        const { house, offset } = this.selectUnassignedVariable();

        // If we ran out of variables, it means the search is a dead end
        // in this branch. Return failure.
        if (!house) {
            return false;
        }

        // Copy list of possible values for that variable.
        const valueBackup = house.variables[offset];
        let possibleValues = valueBackup;

        // Go over possible values and try to solve.
        let puzzleBackup = undefined;
        while (possibleValues !== 0) {
            // Get next possible value and save it to the variable.
            const value = firstBit(possibleValues);
            possibleValues &= ~value;
            house.variables[offset] = value;

            // Make sure the puzzle configuration is valid with this value.
            if (this.valid) {
                // Attempt to add inferences to the puzzle config.
                // If it works, try solving the puzzle with this variable value.
                puzzleBackup = puzzleBackup || new Puzzle();
                puzzleBackup.assign(this);
                if (addInferences(this) && this.backtrackingSolve()) {
                    return true;
                }

                // Puzzle wasn't solved; remove inferences from the config.
                this.assign(puzzleBackup);
            }

            // Puzzle wasn't solved with this value; we need to try the next one.
        }

        // If we reach this point, it means we ran out of possible values
        // for the variable we selected. Thus, the search is a dead end in this branch.
        // Restore the variable to its original value and return failure.
        house.variables[offset] = valueBackup;
        return false;
    }

    // Selects the next unassigned variable in the puzzle and returns the house and its offset.
    selectUnassignedVariable() {
        // We'll use the Minimum-Remaining-Values (MRV) heuristic to select the next variable:
        // we prioritize variables with the least remaining possible values.
        let house = undefined;
        let offset = undefined;
        let numValues = Number.MAX_VALUE;

        // Iterate houses to find an unassigned variable.
        for (const vHouse of this.houses) {
            // Iterate variables for this house.
            for (let vOffset = 0; vOffset < NUM_VARIABLES; ++vOffset) {
                // Only select variables that still have more than one possible value.
                const vNumValues = popCount(vHouse.variables[vOffset]);
                if (vNumValues > 1 && vNumValues < numValues) {
                    // This variable has fewer remaining values than the one we had, use it.
                    house = vHouse;
                    offset = vOffset;
                    numValues = vNumValues;
                }
            }
        }

        return { house, offset };
    }
}
