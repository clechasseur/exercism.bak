import java.util.Arrays;
import java.util.List;

abstract class LineProcessors {
    private static final List<LineProcessor> PROCESSORS = Arrays.asList(
            new HeaderLineProcessor(),
            new ListItemLineProcessor(),
            new ParagraphLineProcessor()
    );

    static List<LineProcessor> getLineProcessors() {
        return PROCESSORS;
    }
}
