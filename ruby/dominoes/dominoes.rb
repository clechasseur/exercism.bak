class Array
  def without_first(e)
    i = index(e)
    return self if i.nil?
    r = dup
    r.delete_at(i)
    r
  end
end

module Dominoes
  def self.chain?(dominoes)
    chains([], dominoes).any? do |chain|
      chain.empty? || chain.first.first == chain.last.last
    end
  end

  private

  def self.chains(chain, dominoes)
    return [chain] if dominoes.empty?
    dominoes.flat_map do |domino|
      if chain.empty?
        ch = [[domino]]
        if domino.first != domino.last
          ch << [domino.reverse]
        end
        ch.flat_map { |c| chains(c, dominoes.without_first(domino)) }
      elsif chain.last.last == domino.first
        chains(chain + [domino], dominoes.without_first(domino))
      elsif chain.last.last == domino.last
        chains(chain + [domino.reverse], dominoes.without_first(domino))
      else
        []
      end
    end
  end
end
