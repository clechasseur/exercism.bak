import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Tournament {
    private static final Pattern RESULT_REGEX = Pattern.compile("^([^;]+);([^;]+);(win|loss|draw)$");

    private Map<String, TeamResults> results = new HashMap<>();

    void applyResults(String results) {
        String[] lines = results.split("\n");
        for (String line : lines) {
            Matcher matcher = RESULT_REGEX.matcher(line);
            if (matcher.find()) {
                String result = matcher.group(3);
                if (result.equals("win")) {
                    recordWin(matcher.group(1), matcher.group(2));
                } else if (result.equals("loss")) {
                    recordWin(matcher.group(2), matcher.group(1));
                } else {
                    recordDraw(matcher.group(1), matcher.group(2));
                }
            }
        }
    }

    String printTable() {
        List<String> lines = new ArrayList<>(
                Collections.singletonList("Team                           | MP |  W |  D |  L |  P"));
        this.results.entrySet().stream()
                               .map(entry -> new TeamWithResults(entry.getKey(), entry.getValue()))
                               .sorted((team1, team2) -> {
                                   int cmp = team2.results.points() - team1.results.points();
                                   if (cmp == 0) {
                                       cmp = team1.teamName.compareTo(team2.teamName);
                                   }
                                   return cmp;
                               }).forEach(team -> lines.add(String.format("%-31s|  %d |  %d |  %d |  %d |  %d",
                                                                            team.teamName, team.results.matchesPlayed(), team.results.wins(),
                                                                            team.results.draws(), team.results.losses(), team.results.points())));
        return String.join("\n", lines) + "\n";
    }

    private void recordWin(String winners, String losers) {
        this.results.computeIfAbsent(winners, n -> new TeamResults()).recordWin();
        this.results.computeIfAbsent(losers, n -> new TeamResults()).recordLoss();
    }

    private void recordDraw(String team1, String team2) {
        this.results.computeIfAbsent(team1, n -> new TeamResults()).recordDraw();
        this.results.computeIfAbsent(team2, n -> new TeamResults()).recordDraw();
    }

    private static final class TeamResults {
        private int wins;
        private int losses;
        private int draws;

        int matchesPlayed() {
            return this.wins + this.losses + this.draws;
        }

        int wins() {
            return this.wins;
        }

        int losses() {
            return this.losses;
        }

        int draws() {
            return this.draws;
        }

        int points() {
            return this.wins * 3 + this.draws;
        }

        void recordWin() {
            ++this.wins;
        }

        void recordLoss() {
            ++this.losses;
        }

        void recordDraw() {
            ++this.draws;
        }
    }

    private static final class TeamWithResults {
        final String teamName;
        final TeamResults results;

        TeamWithResults(String teamName, TeamResults results) {
            this.teamName = teamName;
            this.results = results;
        }
    }
}
