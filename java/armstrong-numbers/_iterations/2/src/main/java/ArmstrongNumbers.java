class ArmstrongNumbers { 
    boolean isArmstrongNumber(int numberToCheck) {
        String numberAsString = String.valueOf(numberToCheck);
        return numberAsString.chars()
                             .map(c -> (int) Math.pow(c - '0', numberAsString.length()))
                             .sum() == numberToCheck;
    }
}
