class BoutiqueInventory
  CHEAP_THRESHOLD = 30.0

  def initialize(items)
    @items = items
  end

  def item_names
    items.map { |item| item[:name] }.sort
  end

  def cheap
    items.filter { |item| item[:price] < CHEAP_THRESHOLD }
  end

  def out_of_stock
    items.filter { |item| item[:quantity_by_size].empty? }
  end

  def stock_for_item(name)
    items.find { |item| item[:name] == name }[:quantity_by_size]
  end

  def total_stock
    items.map { |item| item[:quantity_by_size].values }.flatten.sum
  end

  private
  attr_reader :items
end
