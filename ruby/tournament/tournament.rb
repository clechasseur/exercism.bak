module Tournament
  HEADER = "Team                           | MP |  W |  D |  L |  P\n"

  class Team
    attr_accessor :wins, :draws, :losses

    def initialize
      @wins = @draws = @losses = 0
    end

    def played
      wins + draws + losses
    end

    def points
      wins * 3 + draws
    end
  end

  def self.tally(input)
    results = Hash.new { |hash, name| hash[name] = Team.new }
    input.lines.each do |game_input|
      if /^(?<home>[\w ]+);(?<away>[\w ]+);(?<result>win|loss|draw)$/ =~ game_input
        home_team = results[home]
        away_team = results[away]
        case result
        when "win"
          home_team.wins += 1
          away_team.losses += 1
        when "loss"
          home_team.losses += 1
          away_team.wins += 1
        when "draw"
          home_team.draws += 1
          away_team.draws += 1
        end
      end
    end

    sorted_results = results.to_a.sort do |(n1, t1), (n2, t2)|
      cmp = t2.points <=> t1.points
      cmp == 0 ? n1 <=> n2 : cmp
    end
    HEADER + sorted_results.map do |(name, team)|
      "#{name.ljust(31, ' ')}| #{team.played.to_s.rjust(2, ' ')} | #{team.wins.to_s.rjust(2, ' ')} | #{team.draws.to_s.rjust(2, ' ')} | #{team.losses.to_s.rjust(2, ' ')} | #{team.points.to_s.rjust(2, ' ')}\n"
    end.join
  end
end
