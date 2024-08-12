type TriangleKind = 'invalid' | 'scalene' | 'isosceles' | 'equilateral';

const uniqueSidesToKind: Record<number, TriangleKind> = {
  1: 'equilateral',
  2: 'isosceles',
  3: 'scalene',
};

export class Triangle {
  private readonly kind: TriangleKind;

  constructor(...sides: number[]) {
    const sortedSides = [...sides].sort((a, b) => a - b);

    if (sortedSides[0] + sortedSides[1] >= sortedSides[2] && sortedSides[0] > 0) {
      this.kind = uniqueSidesToKind[new Set<number>(sortedSides).size];
    } else {
      this.kind = 'invalid';
    }
  }

  get isEquilateral(): boolean {
    return this.kind === 'equilateral';
  }

  get isIsosceles(): boolean {
    return this.isEquilateral || this.kind === 'isosceles';
  }

  get isScalene(): boolean {
    return this.kind === 'scalene';
  }
}
