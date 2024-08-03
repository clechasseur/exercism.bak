import java.util.HashMap;
import java.util.Map;

class TwelveDays {
    private static final Map<Integer, VerseInfo> VERSES = new HashMap<>() {{
        put(1, new VerseInfo("first", "a Partridge in a Pear Tree"));
        put(2, new VerseInfo("second", "two Turtle Doves"));
        put(3, new VerseInfo("third", "three French Hens"));
        put(4, new VerseInfo("fourth", "four Calling Birds"));
        put(5, new VerseInfo("fifth", "five Gold Rings"));
        put(6, new VerseInfo("sixth", "six Geese-a-Laying"));
        put(7, new VerseInfo("seventh", "seven Swans-a-Swimming"));
        put(8, new VerseInfo("eighth", "eight Maids-a-Milking"));
        put(9, new VerseInfo("ninth", "nine Ladies Dancing"));
        put(10, new VerseInfo("tenth", "ten Lords-a-Leaping"));
        put(11, new VerseInfo("eleventh", "eleven Pipers Piping"));
        put(12, new VerseInfo("twelfth", "twelve Drummers Drumming"));
    }};

    String verse(int verseNumber) {
        VerseInfo verseInfo = VERSES.get(verseNumber);
        if (verseInfo == null) {
            throw new IllegalArgumentException("Invalid verse number: " + verseNumber);
        }

        StringBuilder sb = new StringBuilder();
        sb.append("On the ").append(verseInfo.getDay()).append(" day of Christmas my true love gave to me: ")
          .append(verseInfo.getGift());
        for (int i = verseNumber - 1; i >= 1; --i) {
            sb.append(", ");
            if (i == 1) {
                sb.append("and ");
            }
            sb.append(VERSES.get(i).getGift());
        }
        sb.append(".\n");
        return sb.toString();
    }

    String verses(int startVerse, int endVerse) {
        StringBuilder sb = new StringBuilder(verse(startVerse));
        for (int i = startVerse + 1; i <= endVerse; ++i) {
            sb.append("\n").append(verse(i));
        }
        return sb.toString();
    }
    
    String sing() {
        return verses(1, 12);
    }

    private static final class VerseInfo {
        private final String day;
        private final String gift;

        VerseInfo(String day, String gift) {
            this.day = day;
            this.gift = gift;
        }

        String getDay() {
            return day;
        }

        String getGift() {
            return gift;
        }
    }
}
