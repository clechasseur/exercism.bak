class Grep
  def self.grep(pattern, flags, files)
    g = Grep.new(pattern, flags, files)
    g.execute
  end

  def initialize(pattern, flags, files)
    parse_flags(flags)
    build_pattern(pattern)
    @files = files
  end

  def execute
    @files.flat_map { |file| grep_in_one_file(file) }.join("\n")
  end

  private

  def parse_flags(flags)
    @line_numbers = flags.include? "-n"
    @only_names = flags.include? "-l"
    @insensitive = flags.include? "-i"
    @invert = flags.include? "-v"
    @match_entire_lines = flags.include? "-x"
  end

  def build_pattern(pattern)
    @pattern = Regexp.new("#{@match_entire_lines ? '^' : ''}#{pattern}#{@match_entire_lines ? '$' : ''}", @insensitive)
  end

  def grep_in_one_file(file)
    matches = IO.readlines(file, chomp: true).each_with_index.filter do |line, *|
      match? line
    end.map do |line, index|
      "#{prefix(file, index)}#{line}"
    end
    return matches.empty? ? [] : [file] if @only_names
    matches
  end

  def match?(line)
    @pattern.match?(line) ^ @invert
  end

  def prefix(file_name, line_index)
    "#{@files.size > 1 ? file_name + ':' : ''}#{@line_numbers ? (line_index + 1).to_s + ':' : ''}"
  end
end
