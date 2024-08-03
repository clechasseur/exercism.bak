export class Bowling {
    constructor() {
        this.frames = [
            new Frame(),
        ];
        this.done = false;
    }

    roll(score) {
        if (this.done) {
            throw new Error('Cannot roll after game is over');
        } else if (score < 0) {
            throw new Error('Negative roll is invalid');
        }

        const frame = this.frames[this.frames.length - 1];
        frame.roll(score);
        if (frame.done) {
            if (this.frames.length < 9) {
                this.frames.push(new Frame());
            } else if (this.frames.length === 9) {
                this.frames.push(new LastFrame());
            } else {
                this.done = true;
            }
        }
    }

    score() {
        if (!this.done) {
            throw new Error('Score cannot be taken until the end of the game')
        }

        return this.frames.map((frame, idx, frames) => {
            const nextTwoThrows = [];
            if (idx !== 10 && (idx  + 1) < frames.length) {
                nextTwoThrows.push(frames[idx + 1].rolls[0]);
                if (frames[idx + 1].rolls.length > 1) {
                    nextTwoThrows.push(frames[idx + 1].rolls[1]);
                } else if ((idx + 2) < frames.length) {
                    nextTwoThrows.push(frames[idx + 2].rolls[0]);
                }
            }
            return frame.score(nextTwoThrows);
        }).reduce((acc, i) => acc + i, 0);
    }
}

class Frame {
    constructor() {
        this.rolls = [];
    }

    get done() {
        return this.rolls.length === 2 || (this.rolls.length === 1 && this.rolls[0] === 10);
    }

    roll(score) {
        if (score + (this.rolls.length > 0 ? this.rolls[0] : 0) > 10) {
            throw new Error('Pin count exceeds pins on the lane');
        }

        this.rolls.push(score);
    }

    score(nextTwoThrows) {
        if (this.rolls[0] === 10) {
            return 10 + nextTwoThrows[0] + nextTwoThrows[1];
        } else if ((this.rolls[0] + this.rolls[1]) === 10) {
            return 10 + nextTwoThrows[0];
        } else {
            return this.rolls[0] + this.rolls[1];
        }
    }
}

class LastFrame {
    constructor() {
        this.rolls = [];
        this.remain = 10;
    }

    get done() {
        return this.rolls.length === 3 || (this.rolls.length === 2 && (this.rolls[0] + this.rolls[1]) < 10);
    }

    roll(score) {
        if (score > this.remain) {
            throw new Error('Pin count exceeds pins on the lane');
        }

        this.rolls.push(score);
        this.remain -= score;
        if (this.remain === 0) {
            this.remain = 10;
        }
    }

    score() {
        return this.rolls.reduce((acc, i) => acc + i, 0);
    }
}
