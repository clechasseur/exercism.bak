import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;

class ListOps {

    static <T> List<T> append(List<T> list1, List<T> list2) {
        return concat(Arrays.asList(list1, list2));
    }

    static <T> List<T> concat(List<List<T>> listOfLists) {
        List<T> result = new ArrayList<>();
        for (List<T> list : listOfLists) {
            result.addAll(list);
        }
        return result;
    }

    static <T> List<T> filter(List<T> list, Predicate<T> predicate) {
        List<T> result = new ArrayList<>();
        for (T value : list) {
            if (predicate.test(value)) {
                result.add(value);
            }
        }
        return result;
    }

    static <T> int size(List<T> list) {
        return list.size();
    }

    static <T, U> List<U> map(List<T> list, Function<T, U> transform) {
        List<U> result = new ArrayList<>();
        for (T value : list) {
            result.add(transform.apply(value));
        }
        return result;
    }

    static <T> List<T> reverse(List<T> list) {
        List<T> result = new ArrayList<>();
        for (int i = size(list) - 1; i >= 0; i--) {
            result.add(list.get(i));
        }
        return result;
    }

    static <T, U> U foldLeft(List<T> list, U initial, BiFunction<U, T, U> f) {
        U result = initial;
        for (T value : list) {
            result = f.apply(result, value);
        }
        return result;
    }

    static <T, U> U foldRight(List<T> list, U initial, BiFunction<T, U, U> f) {
        return foldLeft(reverse(list), initial, (acc, v) -> f.apply(v, acc));
    }

    private ListOps() {
        // No instances.
    }

}
