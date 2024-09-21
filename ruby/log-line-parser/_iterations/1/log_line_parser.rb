class LogLineParser
  LOG_FORMAT = /\A\[(?<level>\w+)\]:\s+(?<message>.*\S)\s*\Z/

  def initialize(line)
    @line = line
    @match = @line.match(LOG_FORMAT)
  end

  def message
    @match[:message]
  end

  def log_level
    @match[:level].downcase
  end

  def reformat
    "#{message} (#{log_level})"
  end
end
