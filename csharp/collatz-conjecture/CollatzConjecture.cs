using System;
using System.Collections.Generic;
using System.Linq;

public static class CollatzConjecture
{
    public static int Steps(int number)
    {
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(number, nameof(number));

        return Collatz(number).Count();
    }

    private static IEnumerable<int> Collatz(int number)
    {
        while (number != 1)
        {
            yield return number;
            number = number % 2 == 0 ? number / 2 : number * 3 + 1;
        }
    }
}
