export class BeerSong {
    static n_bottles(n, startOfSentence = false) {
        return `${n === 0 ? (startOfSentence ? 'No more' : 'no more') : n} bottle${n === 1 ? '' : 's'}`;
    }

    static beginOfSecondVerse(n) {
        if (n === 0) {
            return 'Go to the store and buy some more';
        } else {
            return `Take ${n === 1 ? 'it' : 'one'} down and pass it around`;
        }
    }

    static *downwardSequence(from, to) {
        for (let i = from; i >= to; i--) {
            yield i;
        }
    }

    static verse(n) {
        return `${BeerSong.n_bottles(n, true)} of beer on the wall, ${BeerSong.n_bottles(n)} of beer.
${BeerSong.beginOfSecondVerse(n)}, ${BeerSong.n_bottles(n === 0 ? 99 : n - 1)} of beer on the wall.
`;
    }

    static sing(from = 99, to = 0) {
        return [...this.downwardSequence(from, to)].map((i) => BeerSong.verse(i)).join('\n');
    }
}
