const DAYS = ['first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth'];
const GIFTS = [
    'a Partridge in a Pear Tree',
    'two Turtle Doves',
    'three French Hens',
    'four Calling Birds',
    'five Gold Rings',
    'six Geese-a-Laying',
    'seven Swans-a-Swimming',
    'eight Maids-a-Milking',
    'nine Ladies Dancing',
    'ten Lords-a-Leaping',
    'eleven Pipers Piping',
    'twelve Drummers Drumming'
];

export function recite(from, to = from) {
    return [...DAYS.keys()].slice(from - 1, to).map((i) => reciteOne(i)).join('\n');
}

function reciteOne(n) {
    let verse = `On the ${DAYS[n]} day of Christmas my true love gave to me: `;
    if (n > 0) {
        verse = `${verse}${[...DAYS.keys()].slice(1, n + 1).reverse().map((i) => GIFTS[i]).join(', ')}, and `;
    }
    verse = `${verse}${GIFTS[0]}.\n`;
    return verse;
}