import java.util.*;

final class State implements Cloneable {
    List<House> houses = new ArrayList<>();

    State() {
        for (int i = 0; i < 5; ++i) {
            this.houses.add(new House(i));
        }
    }

    boolean valid() {
        return houses.stream().allMatch(House::valid);
    }

    House getWith(String param, String value) {
        return houses.stream()
                .filter(house -> value.equals(house.get(param)))
                .findFirst().orElse(null);
    }

    void set(House house, String param, String value) {
        house.set(param, value);
        Queue<HouseParamInfo> toRemove = new LinkedList<>();
        toRemove.add(new HouseParamInfo(house, param, value));
        while (!toRemove.isEmpty()) {
            HouseParamInfo houseParamInfo = toRemove.remove();
            houses.stream().filter(h -> h != houseParamInfo.house).forEach(h -> {
                if (h.couldBe(houseParamInfo.param, houseParamInfo.value)) {
                    h.remove(houseParamInfo.param, houseParamInfo.value);
                    String singleValue = h.get(houseParamInfo.param);
                    if (singleValue != null) {
                        toRemove.add(new HouseParamInfo(h, houseParamInfo.param, singleValue));
                    }
                }
            });
        }
    }

    void remove(House house, String param, String value) {
        house.remove(param, value);
        String isThen = house.get(param);
        if (isThen != null) {
            set(house, param, isThen);
        }
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        State state = (State) super.clone();
        state.houses = new ArrayList<>(houses);
        state.houses.replaceAll(h -> {
            try {
                return (House) h.clone();
            } catch (CloneNotSupportedException cnse) {
                throw new IllegalStateException(cnse);
            }
        });
        return state;
    }

    private static final class HouseParamInfo {
        final House house;
        final String param;
        final String value;

        HouseParamInfo(House house, String param, String value) {
            this.house = house;
            this.param = param;
            this.value = value;
        }
    }
}
