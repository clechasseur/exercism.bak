const DAYS = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
const NTH_WHICH = ['1st', '2nd', '3rd', '4th', '5th'];

function lastDayOfMonth(year, monthIndex) {
    if (monthIndex == 1) {
        return (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) ? 29 : 28;
    } else if ([0, 2, 4, 6, 7, 9, 11].indexOf(monthIndex) != -1) {
        return 31;
    } else {
        return 30;
    }
}

function meetupOnNthWeekday(year, monthIndex, dayIndex, whichIndex) {
    let seen = 0;
    for (let day = 1; day <= lastDayOfMonth(year, monthIndex); day++) {
        const date = new Date(year, monthIndex, day);
        if (date.getDay() === dayIndex) {
            seen++;
            if (seen > whichIndex) {
                return date;
            }
        }
    }
    throw new Error('Non-existent day');
}

function meetupOnLastWeekday(year, monthIndex, dayIndex) {
    for (let day = lastDayOfMonth(year, monthIndex); day >= 1; day--) {
        const date = new Date(year, monthIndex, day);
        if (date.getDay() === dayIndex) {
            return date;
        }
    }
    throw new Error('Non-existent day (???)');
}

function meetupOnTeenthWeekday(year, monthIndex, dayIndex) {
    for (let day = 13; day <= 19; day++) {
        const date = new Date(year, monthIndex, day);
        if (date.getDay() === dayIndex) {
            return date;
        }
    }
    throw new Error('Non-existent day (???)');
}

export function meetupDay(year, monthIndex, day, which) {
    const dayIndex = DAYS.indexOf(day);
    const whichIndex = NTH_WHICH.indexOf(which);
    if (whichIndex !== -1) {
        return meetupOnNthWeekday(year, monthIndex, dayIndex, whichIndex);
    } else if (which == 'last') {
        return meetupOnLastWeekday(year, monthIndex, dayIndex);
    } else if (which == 'teenth') {
        return meetupOnTeenthWeekday(year, monthIndex, dayIndex);
    } else {
        throw new Error('Invalid which: ' + which);
    }
}
