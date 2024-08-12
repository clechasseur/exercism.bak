require 'set'

module BookStore
  def self.calculate_price(basket)
    # The algorithm used to form groups (below) works, I think, but is too slow
    # unless I limit the number of groups to be formed. However, this causes some
    # tests not to pass. The solution is to sort the initial basket by reverse order
    # of number of books by type. This is definitely a smell, but it works :D
    basket = basket.group_by(&:itself).values.sort_by { |books| -books.size }.flatten

    form_groups(basket, BookGroups.new).map(&:cost).min
  end

  private

  BOOK_COST = 8.0
  DISCOUNTS = [0.0, 0.0, 0.05, 0.1, 0.2, 0.25]

  def self.form_groups(basket, book_groups)
    return [book_groups] if basket.empty?

    next_book = basket.first
    next_basket = basket.drop(1)
    
    seen_groups = Set.new
    r = book_groups.groups.flat_map.with_index do |group, index|
      if group.can_add?(next_book) && seen_groups.add?(group)
        next_groups = book_groups.clone
        next_groups.groups[index].add(next_book)
        form_groups(next_basket, next_groups)
      else
        []
      end
    end

    if r.all?(&:at_most_one_group_of_one?)
      next_groups = book_groups.clone
      next_groups.add_new_group
      next_groups.groups.last.add(next_book)
      r.append(*form_groups(next_basket, next_groups))
    end

    r
  end

  class BookGroup
    attr_reader :books

    def initialize
      @books = Set.new
    end

    def initialize_copy(other)
      @books = other.books.clone
    end

    def ==(other)
      books == other.books
    end

    def hash
      books.hash
    end

    def can_add?(book)
      !books.include?(book)
    end

    def add(book)
      books.add(book)
    end

    def size
      books.size
    end

    def cost
      books.size * BOOK_COST * (1.0 - DISCOUNTS[books.size])
    end

    def to_s
      "{#{books.map(&:to_s).join(', ')}}"
    end

    alias :eql? :==
  end

  class BookGroups
    attr_reader :groups

    def initialize
      @groups = []
    end

    def initialize_copy(other)
      @groups = other.groups.map(&:clone)
    end

    def ==(other)
      groups == other.groups
    end

    def hash
      groups.hash
    end

    def add_new_group
      groups << BookGroup.new
    end

    def at_most_one_group_of_one?
      groups.count { _1.size == 1 } <= 1
    end

    def cost
      groups.sum(&:cost)
    end

    def to_s
      "[#{groups.map(&:to_s).join(', ')}]"
    end

    alias :eql? :==
  end
end
