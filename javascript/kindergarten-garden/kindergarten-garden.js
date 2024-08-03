const DEFAULT_STUDENTS = ['alice', 'bob', 'charlie', 'david', 'eve', 'fred', 'ginny', 'harriet', 'ileana', 'joseph', 'kincaid', 'larry'];
const PLANTS = { V: 'violets', R: 'radishes', C: 'clover', G: 'grass' };

export class Garden {
    constructor(input, students = DEFAULT_STUDENTS) {
        const splitRows = input.split('\n').map((row) => [...row]);
        students.sort().forEach((student, idx) => {
            this[student.toLowerCase()] = [splitRows[0][idx * 2],
                                           splitRows[0][(idx * 2) + 1],
                                           splitRows[1][idx * 2],
                                           splitRows[1][(idx * 2) + 1]].map((plantLetter) => PLANTS[plantLetter]);
        });
    }
}
