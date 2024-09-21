export const solve = (x, y) => {
    var dist = Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
    if (dist <= 1) {
        return 10;
    } else if (dist <= 5) {
        return 5;
    } else if (dist <= 10) {
        return 1;
    } else {
        return 0;
    }
}
