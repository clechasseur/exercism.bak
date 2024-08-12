import java.util.stream.Collectors;
import java.util.stream.IntStream;

class BeerSong {
    String sing(int startBeers, int numVerses) {
        return IntStream.iterate(startBeers, beers -> beers > startBeers - numVerses, beers -> beers - 1)
                        .mapToObj(this::sing)
                        .collect(Collectors.joining());
    }

    String singSong() {
        return sing(99, 100);
    }

    private String sing(int beers) {
        if (beers > 0) {
            return String.format("%1$d bottle%2$s of beer on the wall, %1$d bottle%2$s of beer.\n" +
                    "Take %3$s down and pass it around, %4$s bottle%5$s of beer on the wall.\n\n",
                    beers,
                    beers > 1 ? "s" : "",
                    beers != 1 ? "one" : "it",
                    beers != 1 ? String.valueOf(beers - 1) : "no more",
                    beers != 2 ? "s" : "");
        } else {
            return "No more bottles of beer on the wall, no more bottles of beer.\n" +
                    "Go to the store and buy some more, 99 bottles of beer on the wall.\n\n";
        }
    }
}
