public class Atbash {
    public String encode(String input) {
        return transpose(input).replaceAll("[a-z0-9]{1,5}", "$0 ").trim();
    }

    public String decode(String input) {
        return transpose(input);
    }

    private String transpose(String input) {
        String cleaned = input.toLowerCase().replaceAll("[^a-z0-9]", "");
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < cleaned.length(); ++i) {
            char c = cleaned.charAt(i);
            if (c >= 'a' && c <= 'z') {
                sb.append((char) ('z' - (c - 'a')));
            } else {
                sb.append(c);
            }
        }
        return sb.toString();
    }
}
