# This is a custom exception that you can use in your code
class NotMovieClubMemberError < RuntimeError
end

class Moviegoer
  TICKET_PRICE = 15
  TICKER_PRICE_SENIORS = 10
  SENIOR_AGE_THRESHOLD = 60
  ADULT_AGE_THRESHOLD = 18

  def initialize(age, member: false)
    @age = age
    @member = member
  end

  def ticket_price
    @age >= SENIOR_AGE_THRESHOLD ? TICKER_PRICE_SENIORS : TICKET_PRICE
  end

  def watch_scary_movie?
    @age >= ADULT_AGE_THRESHOLD
  end

  # Popcorn is 🍿
  def claim_free_popcorn!
    raise NotMovieClubMemberError, "Only Movie Club members get free popcorn" unless @member
    "🍿"
  end
end
