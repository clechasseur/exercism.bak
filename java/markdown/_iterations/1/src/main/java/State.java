import java.util.ArrayDeque;
import java.util.Deque;

final class State {
    StringBuilder htmlBuilder = new StringBuilder();
    Deque<String> endTags = new ArrayDeque<>();
}
