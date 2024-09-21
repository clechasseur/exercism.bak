export function proverb(...things) {
    let qualifiedThing = things[0];
    if (typeof things[things.length - 1] === 'object') {
        qualifiedThing = `${things[things.length - 1].qualifier} ${things[0]}`;
        things = things.slice(0, things.length - 1);
    }
    return [...things.keys()].slice(1)
                             .map((i) => `For want of a ${things[i - 1]} the ${things[i]} was lost.\n`)
                             .join('') + `And all for the want of a ${qualifiedThing}.`;
}
