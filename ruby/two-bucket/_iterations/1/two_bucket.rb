require 'set'

class TwoBucket
  BUCKET_NAMES = ["one", "two"]

  attr_reader :moves, :goal_bucket, :other_bucket

  def initialize(bucket_one_size, bucket_two_size, desired_litres, start_with)
    params = Params.new(bucket_one_size, bucket_two_size, desired_litres, start_with)
    states = [State.new([0, 0]).fill_bucket(params, params.start_bucket_index)].to_set
    @moves = 1
    while @goal_bucket.nil?
      wincons = states.map { |s| s.wincon(params) }.compact
      if !wincons.empty?
        @goal_bucket, @other_bucket = wincons.first
      else
        states = states.flat_map { |s| s.next_states(params) }.to_set
        @moves += 1
      end
    end
  end

  private

  class Params
    attr_reader :bucket_sizes, :desired_litres, :start_bucket_index, :other_bucket_index

    def initialize(bucket_one_size, bucket_two_size, desired_litres, start_with)
      @bucket_sizes = [bucket_one_size, bucket_two_size]
      @desired_litres = desired_litres
      @start_bucket_index = BUCKET_NAMES.index(start_with)
      @other_bucket_index = (start_bucket_index - 1).abs
    end
  end

  class State
    attr_reader :buckets

    def initialize(buckets)
      @buckets = buckets
    end

    def hash
      buckets.hash
    end

    def eql?(other)
      buckets.eql?(other.buckets)
    end

    def valid?(params)
      buckets[params.start_bucket_index] > 0 || buckets[params.other_bucket_index] < params.bucket_sizes[params.other_bucket_index]
    end

    def wincon(params)
      winner = buckets.index(params.desired_litres)
      return nil if winner.nil?
      loser = (winner - 1).abs
      [BUCKET_NAMES[winner], buckets[loser]]
    end

    def fill_bucket(params, bucket_index)
      State.new(buckets.map.with_index do |b, i|
        i == bucket_index ? params.bucket_sizes[i] : b
      end)
    end

    def empty_bucket(bucket_index)
      State.new(buckets.map.with_index do |b, i|
        i == bucket_index ? 0 : b
      end)
    end

    def pour_bucket(params, source_bucket_index)
      dest_bucket_index = (source_bucket_index - 1).abs
      room = params.bucket_sizes[dest_bucket_index] - buckets[dest_bucket_index]
      avail = buckets[source_bucket_index]
      poured = [room, avail].min
      new_buckets = [0, 0]
      new_buckets[source_bucket_index] = buckets[source_bucket_index] - poured
      new_buckets[dest_bucket_index] = buckets[dest_bucket_index] + poured
      State.new(new_buckets)
    end

    def next_states(params)
      (0...buckets.size).flat_map do |i|
        [fill_bucket(params, i), empty_bucket(i), pour_bucket(params, i)]
      end.filter do |state|
        state.valid?(params) && !state.eql?(self)
      end
    end
  end
end
