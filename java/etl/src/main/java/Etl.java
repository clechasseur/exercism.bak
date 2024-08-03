import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class Etl {
    Map<String, Integer> transform(Map<Integer, List<String>> old) {
        return old.keySet()
                  .stream()
                  .flatMap(score -> old.get(score)
                                       .stream()
                                       .map(letter -> new KeyValuePair<>(letter.toLowerCase(), score)))
                  .collect(Collectors.toMap(kvPair -> kvPair.key, kvPair -> kvPair.value));
    }

    private static final class KeyValuePair<K, V> {
        final K key;
        final V value;

        KeyValuePair(K key, V value) {
            this.key = key;
            this.value = value;
        }
    }
}
