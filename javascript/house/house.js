export class House {
    constructor() {
        this.entities = [
            'malt',
            'rat',
            'cat',
            'dog',
            'cow with the crumpled horn',
            'maiden all forlorn',
            'man all tattered and torn',
            'priest all shaven and shorn',
            'rooster that crowed in the morn',
            'farmer sowing his corn',
            'horse and the hound and the horn',
        ];
        this.actions = [
            'lay in',
            'ate',
            'killed',
            'worried',
            'tossed',
            'milked',
            'kissed',
            'married',
            'woke',
            'kept',
            'belonged to',
        ];
    }

    static verse(n) {
        const house = new House();
        let output = 'This is the ';
        for (let i = n - 2; i >= 0; i--) {
            output = `${output}${house.entities[i]}\nthat ${house.actions[i]} the `;
        }
        output = `${output}house that Jack built.`;
        return output.split('\n');
    }

    static verses(first, last) {
        let output = [];
        for (let i = first; i <= last; i++) {
            output = output.concat(House.verse(i), ['']);
        }
        output.pop();
        return output;
    }
}
