import java.util.Random;
import java.util.stream.Stream;

class DnDCharacter {
    private final Random random = new Random();

    private final int strength = ability();
    private final int dexterity = ability();
    private final int constitution = ability();
    private final int intelligence = ability();
    private final int wisdom = ability();
    private final int charisma = ability();

    int ability() {
        return Stream.generate(() -> random.nextInt(6) + 1)
                     .limit(4)
                     .sorted()
                     .skip(1)
                     .mapToInt(i -> i)
                     .sum();
    }

    int modifier(int input) {
        return Math.floorDiv(input - 10, 2);
    }

    int getStrength() {
        return strength;
    }

    int getDexterity() {
        return dexterity;
    }

    int getConstitution() {
        return constitution;
    }

    int getIntelligence() {
        return intelligence;
    }

    int getWisdom() {
        return wisdom;
    }

    int getCharisma() {
        return charisma;
    }

    int getHitpoints() {
        return 10 + modifier(getConstitution());
    }
}
