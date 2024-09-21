class Reactor
  def self.is_criticality_balanced?(temperature, neutrons_emitted)
    temperature < 800 && neutrons_emitted > 500 && (temperature * neutrons_emitted) < 500_000
  end

  def self.reactor_efficiency(voltage, current, theoretical_max_power)
    percentage_value = voltage * current / theoretical_max_power * 100
    if percentage_value >= 80.0
      "green"
    elsif percentage_value >= 60.0
      "orange"
    elsif percentage_value >= 30.0
      "red"
    else
      "black"
    end
  end

  def self.fail_safe(temperature, neutrons_produced_per_second, threshold)
    power = temperature * neutrons_produced_per_second
    if power < (threshold * 0.9)
      "LOW"
    elsif ((threshold - power) / threshold).abs <= 0.1
      "NORMAL"
    else
      "DANGER"
    end
  end
end
