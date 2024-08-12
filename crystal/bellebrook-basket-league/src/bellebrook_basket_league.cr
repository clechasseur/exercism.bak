module TicketingReservation
  def tickets_available
    @tickets_available
  end

  def order_ticket?
    can_order = tickets_available >= 100
    @tickets_available -= 1 if can_order
    can_order
  end

  def order_message(name)
    ticket_number = tickets_available
    if order_ticket?
      "#{name}, your purchase was successful, your ticket number is \##{ticket_number}, " +
        "and the game is played at the #{@stadium} stadium."
    else
      "#{name}, your purchase was unsuccessful, there are not enough tickets available."
    end
  end
end

class TicketSystem
  include TicketingReservation

  def initialize(tickets_available : Int32, stadium : String)
    @tickets_available = tickets_available
    @stadium = stadium
  end
end
