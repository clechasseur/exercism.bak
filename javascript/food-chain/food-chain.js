export class Song {
    constructor() {
        this.animals = [
            'fly',
            'spider',
            'bird',
            'cat',
            'dog',
            'goat',
            'cow',
            'horse',
        ];
        this.animals_to_catch = [
            'fly',
            'spider that wriggled and jiggled and tickled inside her',
            'bird',
            'cat',
            'dog',
            'goat',
            null,
            null,
        ];
        this.animal_verses = [
            null,
            'It wriggled and jiggled and tickled inside her.',
            'How absurd to swallow a bird!',
            'Imagine that, to swallow a cat!',
            'What a hog, to swallow a dog!',
            'Just opened her throat and swallowed a goat!',
            'I don\'t know how she swallowed a cow!',
            null,
        ];
    }

    verse(n) {
        n--;
        let output = `I know an old lady who swallowed a ${this.animals[n]}.\n`;
        if (n != 7) {
            if (n > 0) {
                output = `${output}${this.animal_verses[n]}\n`;
                for (let i = n; i > 0; i--) {
                    output = `${output}She swallowed the ${this.animals[i]} to catch the ${this.animals_to_catch[i - 1]}.\n`;
                }
            }
            output = `${output}I don't know why she swallowed the ${this.animals[0]}. Perhaps she'll die.\n`;
        } else {
            output = `${output}She's dead, of course!\n`;
        }
        return output;
    }

    verses(first, last) {
        let output = "";
        for (let i = first; i <= last; i++) {
            output = `${output}${this.verse(i)}\n`;
        }
        return output;
    }
}
