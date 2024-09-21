class Node
  attr_reader :value, :left, :right, :up

  def initialize(value, left, right)
    set_value(value)
    set_left(left)
    set_right(right)
  end

  def ==(other)
    value == other.value && left == other.left && right == other.right
  end

  def to_tree
    tree = self
    tree = tree.up until tree.up.nil?
    tree
  end

  def set_value(value)
    @value = value
    self
  end
  
  def set_up(up)
    @up = up
    self
  end

  def set_left(left)
    @left = left
    @left.set_up(self) unless @left.nil?
    self
  end

  def set_right(right)
    @right = right
    @right.set_up(self) unless @right.nil?
    self
  end
end

module Zipper
  def self.from_tree(tree)
    tree
  end
end
