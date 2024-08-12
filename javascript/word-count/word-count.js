export class Words {
    count(input) {
        const cleanedInput = input.toLowerCase();
        const output = {};
        const regex = /\S+/g;
        let matches;
        while ((matches = regex.exec(cleanedInput)) !== null) {
            const [word] = matches;
            if (!(word in output) || output[word] instanceof Function) {
                output[word] = 0;
            }
            output[word]++;
        }
        return output;
    }
}
