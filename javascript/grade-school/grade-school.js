export class GradeSchool {
    constructor() {
        this._roster = {};
    }

    roster() {
        const roster = {};
        for (const grade in this._roster) {
            roster[grade] = Array.from(this._roster[grade]);
        }
        return roster;
    }

    add(student, grade) {
        if (!(grade in this._roster)) {
            this._roster[grade] = [student];
        } else {
            this._roster[grade].push(student);
            this._roster[grade].sort();
        }
    }

    grade(grade) {
        return grade in this._roster ? Array.from(this._roster[grade]) : [];
    }
}
