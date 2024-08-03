export class Triplet {
    constructor(a, b, c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    sum() {
        return this.a + this.b + this.c;
    }

    product() {
        return this.a * this.b * this.c;
    }

    isPythagorean() {
        return this.a < this.b && this.b < this.c && (this.a**2 + this.b**2) === this.c**2;
    }

    static where(criteria) {
        const triplets = [];
        const minFactor = criteria.minFactor || 1;
        const maxFactor = criteria.maxFactor || 1000;
        const matchesSum = (triplet) => criteria.sum ? triplet.sum() === criteria.sum : true;
        for (let c = minFactor + 2; c <= maxFactor; c++) {
            for (let b = minFactor + 1; b < c; b++) {
                for (let a = minFactor; a < b; a++) {
                    const triplet = new Triplet(a, b, c);
                    if (triplet.isPythagorean() && matchesSum(triplet)) {
                        triplets.push(triplet);
                    }
                }
            }
        }
        return triplets;
    }

    toString() {
        return this.product().toString().padStart(16, '0');
    }
}
