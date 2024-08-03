export class HighScores {
    constructor(scores) {
        this.scores = scores;
    }

    get latest() {
        return this.scores[this.scores.length - 1];
    }

    get personalBest() {
        return this.scores.reduce((a, b) => a > b ? a : b, 0);
    }

    get personalTopThree() {
        return this.scores.sort((a, b) => b - a).slice(0, 3);
    }
}
