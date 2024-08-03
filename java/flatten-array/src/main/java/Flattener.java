import java.util.ArrayList;
import java.util.List;

public class Flattener {
    public List<?> flatten(List<?> list) {
        List<Object> result = new ArrayList<>();
        for (Object e : list) {
            if (e instanceof List<?>) {
                result.addAll(flatten((List<?>) e));
            } else if (e != null) {
                result.add(e);
            }
        }
        return result;
    }
}
