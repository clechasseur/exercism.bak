import java.util.*;

final class House implements Cloneable {
    final int idx;
    Map<String, Set<String>> params = new HashMap<>() {{
        put("owner", new HashSet<>() {{
            add("Englishman");
            add("Spaniard");
            add("Ukrainian");
            add("Norwegian");
            add("Japanese");
        }});
        put("color", new HashSet<>() {{
            add("red");
            add("green");
            add("ivory");
            add("yellow");
            add("blue");
        }});
        put("pet", new HashSet<>() {{
            add("dog");
            add("snails");
            add("fox");
            add("horse");
            add("zebra");
        }});
        put("beverage", new HashSet<>() {{
            add("coffee");
            add("tea");
            add("milk");
            add("orange juice");
            add("water");
        }});
        put("cigarettes", new HashSet<>() {{
            add("Old Gold");
            add("Kools");
            add("Chesterfields");
            add("Lucky Strike");
            add("Parliaments");
        }});
    }};

    House(int idx) {
        this.idx = idx;
    }

    boolean valid() {
        return params.keySet().stream().noneMatch(p -> params.get(p).isEmpty());
    }

    String get(String param) {
        Set<String> set = params.get(param);
        if (set.size() == 1) {
            return set.iterator().next();
        }
        return null;
    }

    boolean couldBe(String param, String value) {
        return params.get(param).contains(value);
    }

    boolean couldBeOtherThan(String param, String... values) {
        return !params.get(param).equals(new HashSet<>(Arrays.asList(values)));
    }

    void set(String param, String... values) {
        params.put(param, new HashSet<>(Arrays.asList(values)));
    }

    void remove(String param, String value) {
        params.get(param).remove(value);
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        House house = (House) super.clone();
        house.params = new HashMap<>(params);
        house.params.replaceAll((p, hs) -> new HashSet<>(hs));
        return house;
    }
}
