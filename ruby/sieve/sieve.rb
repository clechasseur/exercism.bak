require 'set'

class Sieve
  def initialize(up_to)
    @up_to = up_to
  end

  def primes
    marked = Set.new
    (2..@up_to).filter do |n|
      marked.merge((n..@up_to).step(n).drop(1))
      !(marked === n)
    end
  end
end
