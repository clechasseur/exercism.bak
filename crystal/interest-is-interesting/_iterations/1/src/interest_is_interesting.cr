class SavingsAccount
  def self.interest_rate(balance)
    if balance.negative?
      3.213
    elsif balance < 1000.0
      0.5
    elsif balance < 5000.0
      1.621
    else
      2.475
    end
  end

  def self.interest(balance)
    balance * interest_rate(balance) / 100
  end

  def self.annual_balance_update(balance)
    balance + interest(balance)
  end

  def self.years_before_desired_balance(current_balance, target_balance)
    years = 0

    while current_balance < target_balance
      current_balance = annual_balance_update(current_balance)
      years += 1
    end

    years
  end
end
