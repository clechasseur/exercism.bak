import java.awt.Point;
import java.util.*;
import java.util.stream.Collectors;

class GoCounting {
    private List<List<Player>> board;
    private int width;
    private int height;
    private Map<Player, Set<Point>> territoriesPerPlayer = new HashMap<>() {{
        for (Player player : Player.values()) {
            put(player, new HashSet<>());
        }
    }};
    private Map<Point, Territory> territoriesPerPoint = new HashMap<>();

    GoCounting(String board) {
        this.board = board.lines().map(line -> line.chars().mapToObj(
                c -> Player.forSymbol((char) c)).collect(Collectors.toList())).collect(Collectors.toList());
        width = !this.board.isEmpty() ? this.board.get(0).size() : 0;
        height = this.board.size();

        List<Territory> territories = new ArrayList<>();
        Set<Point> toCheck = new HashSet<>();
        for (int x = 0; x < width; x++) {
            for (int y = 0; y < height; y++) {
                toCheck.add(new Point(x, y));
            }
        }
        while (!toCheck.isEmpty()) {
            Point point = toCheck.iterator().next();
            if (at(point) != Player.NONE) {
                toCheck.remove(point);
            } else {
                Territory territory = getTerritoryContaining(point);
                territories.add(territory);
                toCheck.removeAll(territory.points);
            }
        }

        for (Territory territory : territories) {
            territoriesPerPlayer.get(territory.owner).addAll(territory.points);
            for (Point point : territory.points) {
                territoriesPerPoint.put(point, territory);
            }
        }
    }

    Player getTerritoryOwner(int x, int y) {
        validate(x, y);
        Territory territory = territoriesPerPoint.get(new Point(x, y));
        return territory != null ? territory.owner : Player.NONE;
    }

    Set<Point> getTerritory(int x, int y) {
        validate(x, y);
        Territory territory = territoriesPerPoint.get(new Point(x, y));
        return territory != null ? territory.points : Collections.emptySet();
    }

    Map<Player, Set<Point>> getTerritories() {
        return territoriesPerPlayer;
    }

    private void validate(int x, int y) {
        if (x < 0 || x >= width || y < 0 || y >= height) {
            throw new IllegalArgumentException("Invalid coordinate");
        }
    }

    private Player at(Point point) {
        return board.get(point.y).get(point.x);
    }

    private Territory getTerritoryContaining(Point point) {
        Territory territory = new Territory();
        Queue<Point> toCheck = new ArrayDeque<>();
        toCheck.add(point);
        while (!toCheck.isEmpty()) {
            Point pt = toCheck.remove();
            Player player = at(pt);
            if (player == Player.NONE) {
                territory.points.add(pt);
                addNeighbour(new Point(pt.x - 1, pt.y), toCheck, territory);
                addNeighbour(new Point(pt.x, pt.y - 1), toCheck, territory);
                addNeighbour(new Point(pt.x + 1, pt.y), toCheck, territory);
                addNeighbour(new Point(pt.x, pt.y + 1), toCheck, territory);
            } else if (territory.owner != player) {
                territory.owner = territory.owner == null ? player : Player.NONE;
            }
        }
        if (territory.owner == null) {
            territory.owner = Player.NONE;
        }
        return territory;
    }

    private void addNeighbour(Point pt, Queue<Point> toCheck, Territory territory) {
        if (pt.x >= 0 && pt.x < width && pt.y >= 0 && pt.y < height && !territory.points.contains(pt)) {
            toCheck.add(pt);
        }
    }

    private static final class Territory {
        Player owner = null;
        Set<Point> points = new HashSet<>();
    }
}