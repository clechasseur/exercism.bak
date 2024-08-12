class IsbnVerifier {
    boolean isValid(String stringToVerify) {
        boolean valid = false;
        if (stringToVerify != null) {
            String withoutDashes = stringToVerify.replace("-", "");
            if (withoutDashes.matches("[0-9]{9}[0-9X]")) {
                int total = 0;
                for (int i = 0; i < 10; ++i) {
                    if (withoutDashes.charAt(i) == 'X') {
                        total += 10;
                    } else {
                        total += Integer.valueOf(withoutDashes.substring(i, i + 1)) * (10 - i);
                    }
                }
                valid = total % 11 == 0;
            }
        }
        return valid;
    }
}
