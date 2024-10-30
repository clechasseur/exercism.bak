module TwoBucket
  # A port of my Ruby solution, with added support for unreachable goals:
  # https://exercism.org/tracks/ruby/exercises/two-bucket/solutions/clechasseur

  enum Bucket
    One
    Two
  end

  struct Result
    property moves, other_bucket, goal_bucket

    def initialize(@moves : UInt32, @other_bucket : UInt32, @goal_bucket : Bucket); end
  end

  def self.measure(bucket_one : UInt32, bucket_two : UInt32, goal : UInt32, start_bucket : Bucket)
    # This check is an optimization because eventually, we would realize
    # the goal is unreachable in the loop below.
    if bucket_one < goal && bucket_two < goal
      raise ArgumentError.new("Unreachable goal")
    end

    params = Params.new(bucket_one, bucket_two, goal, start_bucket)
    states = [State.new([0_u32, 0_u32]).fill_bucket(params, params.start_bucket_index)].to_set
    seen = states.to_set
    moves = 1_u32

    loop do
      wincons = states.map(&.wincon(params, moves)).compact!
      return wincons.first unless wincons.empty?

      states = states.flat_map(&.next_states(params)).to_set - seen
      raise ArgumentError.new("Unreachable goal") if states.empty?

      seen |= states
      moves += 1
    end
  end

  private BUCKETS = Bucket.values

  private struct Params
    @start_bucket_index : Int32

    property bucket_sizes, goal, start_bucket_index

    def initialize(bucket_one : UInt32, bucket_two : UInt32, @goal : UInt32, start_bucket)
      @bucket_sizes = [bucket_one, bucket_two]
      @start_bucket_index = BUCKETS.index(start_bucket) || raise ArgumentError.new
    end

    def other_bucket_index : Int32
      start_bucket_index - 1
    end
  end

  private struct State
    property buckets

    def initialize(@buckets : Array(UInt32)); end

    def valid?(params : Params)
      buckets[params.start_bucket_index] > 0 || buckets[params.other_bucket_index] < params.bucket_sizes[params.other_bucket_index]
    end

    def wincon(params : Params, moves : UInt32) : Result | Nil
      winner_index = buckets.index(params.goal)
      return nil if winner_index.nil?

      Result.new(
        moves: moves,
        other_bucket: buckets[winner_index - 1],
        goal_bucket: BUCKETS[winner_index],
      )
    end

    def fill_bucket(params : Params, bucket_index : Int32) : self
      State.new(buckets.map_with_index { |b, i| i == bucket_index ? params.bucket_sizes[i] : b })
    end

    def empty_bucket(bucket_index : Int32) : self
      State.new(buckets.map_with_index { |b, i| i == bucket_index ? 0_u32 : b })
    end

    def pour_bucket(params : Params, source_bucket_index : Int32) : self
      dest_bucket_index = source_bucket_index - 1
      room = params.bucket_sizes[dest_bucket_index] - buckets[dest_bucket_index]
      avail = buckets[source_bucket_index]
      poured = [room, avail].min
      new_buckets = [0_u32, 0_u32]
      new_buckets[source_bucket_index] = buckets[source_bucket_index] - poured
      new_buckets[dest_bucket_index] = buckets[dest_bucket_index] + poured
      State.new(new_buckets)
    end

    def next_states(params : Params) : Array(self)
      (0...buckets.size).flat_map { |i| next_states_for(params, i) }.select { |s| s.valid?(params) && s != self }
    end

    private def next_states_for(params : Params, bucket_index : Int32) : Array(self)
      [
        fill_bucket(params, bucket_index),
        empty_bucket(bucket_index),
        pour_bucket(params, bucket_index),
      ]
    end
  end
end
