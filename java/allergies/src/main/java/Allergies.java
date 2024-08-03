import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Allergies {
    private final List<Allergen> allergies;

    public Allergies(int score) {
        List<Allergen> allergies = new ArrayList<>();
        for (Allergen allergen : Allergen.values()) {
            if ((score & allergen.getScore()) != 0) {
                allergies.add(allergen);
            }
        }

        this.allergies = Collections.unmodifiableList(allergies);
    }

    public Boolean isAllergicTo(Allergen allergen) {
        return allergies.contains(allergen);
    }

    public List<Allergen> getList() {
        return allergies;
    }
}
