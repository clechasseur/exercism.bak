class Clock {
    constructor(hours, minutes) {
        this.hours = hours;
        this.minutes = minutes;
        this.adjust();
    }

    adjust() {
        while (this.minutes < 0) {
            this.minutes += 60;
            this.hours--;
        }
        while (this.minutes >= 60) {
            this.minutes -= 60;
            this.hours++;
        }

        while (this.hours < 0) {
            this.hours += 24;
        }
        while (this.hours >= 24) {
            this.hours -= 24;
        }
    }

    toString() {
        return `${this.hours < 10 ? '0' : ''}${this.hours}:${this.minutes < 10 ? '0' : ''}${this.minutes}`;
    }

    plus(minutes) {
        this.minutes += minutes;
        this.adjust();
        return this;
    }
    minus(minutes) {
        this.minutes -= minutes;
        this.adjust();
        return this;
    }

    equals(other) {
        if (!(other instanceof Clock)) {
            return false;
        }
        return this.hours === other.hours && this.minutes === other.minutes;
    }
}

export function at(hours = 0, minutes = 0) {
    return new Clock(hours, minutes);
}
