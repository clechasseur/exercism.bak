function pourOneIntoTwo({ bucketOne, bucketTwo, moves }, capacityTwo) {
  const toPour = Math.min(bucketOne, capacityTwo - bucketTwo);
  return {
    bucketOne: bucketOne - toPour,
    bucketTwo: bucketTwo + toPour,
    moves: moves.concat('1->2'),
  };
}
function pourTwoIntoOne({ bucketOne, bucketTwo, moves }, capacityOne) {
  const toPour = Math.min(bucketTwo, capacityOne - bucketOne);
  return {
    bucketOne: bucketOne + toPour,
    bucketTwo: bucketTwo - toPour,
    moves: moves.concat('2->1'),
  };
}
function emptyOne({ bucketTwo, moves }) {
  return {
    bucketOne: 0,
    bucketTwo,
    moves: moves.concat('E1'),
  };
}
function emptyTwo({ bucketOne, moves }) {
  return {
    bucketOne,
    bucketTwo: 0,
    moves: moves.concat('E2'),
  };
}
function fillOne({ bucketTwo, moves }, capacityOne) {
  return {
    bucketOne: capacityOne,
    bucketTwo,
    moves: moves.concat('F1'),
  };
}
function fillTwo({ bucketOne, moves }, capacityTwo) {
  return {
    bucketOne,
    bucketTwo: capacityTwo,
    moves: moves.concat('F2'),
  };
}

export class TwoBucket {
  constructor(capacityOne, capacityTwo, goal, starterBucket) {
    this._states = [
      {
        bucketOne: starterBucket === 'one' ? capacityOne : 0,
        bucketTwo: starterBucket === 'two' ? capacityTwo : 0,
        moves: [],
      }
    ];
    this._moves = 0;
    while (this._moves === 0) {
      this._states.forEach((state) => {
        if (state.bucketOne === goal) {
          this._moves = state.moves.length + 1;
          this.goalBucket = 'one';
          this.otherBucket = state.bucketTwo;
        } else if (state.bucketTwo === goal) {
          this._moves = state.moves.length + 1;
          this.goalBucket = 'two';
          this.otherBucket = state.bucketOne;
        }
      });
      if (this._moves === 0) {
        this._states = [].concat(...this._states.map((state) => {
          return [
            pourOneIntoTwo(state, capacityTwo),
            pourTwoIntoOne(state, capacityOne),
            emptyOne(state),
            emptyTwo(state),
            fillOne(state, capacityOne),
            fillTwo(state, capacityTwo),
          ];
        })).sort((s1, s2) => {
          let cmp = s1.bucketOne - s2.bucketOne;
          if (cmp === 0) {
            cmp = s1.bucketTwo - s2.bucketTwo;
          }
          return cmp;
        }).filter((state, idx, arr) => {
          if (idx === 0) {
            return true;
          } else {
            const prevState = arr[idx - 1];
            return state.bucketOne !== prevState.bucketOne
                || state.bucketTwo !== prevState.bucketTwo;
          }
        });
      }
    }
  }

  moves() {
    return this._moves;
  }
}
