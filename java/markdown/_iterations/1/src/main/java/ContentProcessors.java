import java.util.Arrays;
import java.util.List;

abstract class ContentProcessors {
    private static final List<ContentProcessor> PROCESSORS = Arrays.asList(
            new RegexContentProcessor("__(.+)__", "<strong>$1</strong>"),
            new RegexContentProcessor("_(.+)_", "<em>$1</em>")
    );

    static List<ContentProcessor> getContentProcessors() {
        return PROCESSORS;
    }
}
