using System;

public static class CollatzConjecture
{
    public static int Steps(int number)
    {
        ArgumentOutOfRangeException.ThrowIfNegativeOrZero(number, nameof(number));

        return tailrecSteps(number, 0);
    }

    // Note: this function would support tail recursion, but unfortunately
    // C# does not support it.
    private static int tailrecSteps(int number, int steps)
    {
        if (number == 1) {
            return steps;
        }

        int newNumber = number % 2 == 0 ? number / 2 : number * 3 + 1;
        return tailrecSteps(newNumber, steps + 1);
    }
}
