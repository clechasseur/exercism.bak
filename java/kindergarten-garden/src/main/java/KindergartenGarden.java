import java.util.*;

class KindergartenGarden {
    private static final int MAX_PLANTS_PER_STUDENT = 4;

    private final Map<Character, List<Plant>> plants = new HashMap<>();

    KindergartenGarden(String garden) {
        garden.lines().forEach(this::addPlantsRow);
    }

    List<Plant> getPlantsOfStudent(String student) {
        return Collections.unmodifiableList(plants.get(student.toUpperCase().charAt(0)));
    }

    private void addPlantsRow(String row) {
        int i = 0;
        while (i < row.length()) {
            final int column = i;
            plants.compute((char) ('A' + (i / 2)), (stu, list) -> {
                if (list == null) {
                    list = new ArrayList<>(MAX_PLANTS_PER_STUDENT);
                }
                list.add(Plant.getPlant(row.charAt(column)));
                list.add(Plant.getPlant(row.charAt(column + 1)));
                return list;
            });
            i += 2;
        }
    }
}
