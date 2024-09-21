require 'set'

module BookStore
  def self.calculate_price(basket)
    form_groups(basket, BookGroups.new).map(&:cost).min
  end

  private

  BOOK_COST = 8.0
  DISCOUNTS = [0.0, 0.0, 0.05, 0.1, 0.2, 0.25]

  def self.form_groups(basket, book_groups)
    return [book_groups] if basket.empty?

    next_book = basket.first
    next_basket = basket.drop(1)
    
    r = book_groups.groups.flat_map.with_index do |group, index|
      if group.can_add? next_book
        next_groups = book_groups.clone
        next_groups.groups[index].add(next_book)
        form_groups(next_basket, next_groups)
      else
        []
      end
    end

    next_groups = book_groups.clone
    next_groups.add_new_group
    next_groups.groups.last.add(next_book)
    r + form_groups(next_basket, next_groups)
  end

  class BookGroup
    attr_reader :books

    def initialize
      @books = Set.new
    end

    def initialize_copy(other)
      @books = other.books.clone
    end

    def can_add?(book)
      !books.include?(book)
    end

    def add(book)
      books.add(book)
    end

    def cost
      books.size * BOOK_COST * (1.0 - DISCOUNTS[books.size])
    end
  end

  class BookGroups
    attr_reader :groups

    def initialize
      @groups = []
    end

    def initialize_copy(other)
      @groups = other.groups.map(&:clone)
    end

    def add_new_group
      @groups << BookGroup.new
    end 

    def cost
      groups.sum(&:cost)
    end
  end
end
