export const translator = {
    vowelSounds: ['a', 'e', 'i', 'o', 'u', 'xr', 'yt'],
    translate: function (phrase) {
        return phrase.replace(/[a-zA-Z]+/g, (word) => this.translateWord(word));
    },
    translateWord: function (word) {
        if (this.vowelSounds.some((sound) => word.startsWith(sound))) {
            return word + 'ay';
        } else {
            const matches = word.match(/^(qu|(?:[^aeiou][^aeiouy]*[^aeiouyq]|[^aeiouq])qu|[^aeiou][^aeiouy]*)(.*)$/i);
            return `${matches[2]}${matches[1]}ay`;
        }
    },
};
