public class Bob {
    public String hey(String input) {
        String cleanedInput = input.trim();
        Boolean question = cleanedInput.endsWith("?");
        Boolean yell = cleanedInput.chars().anyMatch(Character::isUpperCase) &&
                       cleanedInput.chars().noneMatch(Character::isLowerCase);
        if (question && yell) {
            return "Calm down, I know what I'm doing!";
        } else if (question) {
            return "Sure.";
        } else if (yell) {
            return "Whoa, chill out!";
        } else if (cleanedInput.chars().allMatch(Character::isWhitespace)) {
            return "Fine. Be that way!";
        } else {
            return "Whatever.";
        }
    }
}
