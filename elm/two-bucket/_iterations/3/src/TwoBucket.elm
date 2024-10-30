module TwoBucket exposing (BucketNumber(..), measure)

import Set exposing (Set)

type BucketNumber
    = One
    | Two

type alias State =
    { moves : Int, bucketOne : Int, bucketTwo : Int }

measure : Int -> Int -> Int -> BucketNumber -> Maybe State
measure bucketOneSize bucketTwoSize goal startBucket =
    let
        initBuckets =
            initialBuckets bucketOneSize bucketTwoSize startBucket
                |> Set.singleton
    in
        tailrecMeasure goal 1 Set.empty initBuckets
            |> Maybe.map (finalState startBucket)

-- Implementation details follow

-- Bucket represents the state of one bucket.
-- It stores the bucket's current number of liters, and its maximum capacity.
type alias Bucket =
    (Int, Int)

-- Buckets represents the two buckets we have in our problem.
-- The start bucket is always the first one.
type alias Buckets =
    (Bucket, Bucket)

-- initialBuckets creates Buckets representing the initial problem state.
-- It initializes the buckets, including filling the start bucket.
initialBuckets : Int -> Int -> BucketNumber -> Buckets
initialBuckets bucketOneSize bucketTwoSize startBucket =
    let
        emptyBucket size =
            (0, size)
        fullBucket size =
            (size, size)
    in
        case startBucket of
            One -> (fullBucket bucketOneSize, emptyBucket bucketTwoSize)
            Two -> (fullBucket bucketTwoSize, emptyBucket bucketOneSize)

-- finalState converts Buckets and a number of moves into the corresponding State.
finalState : BucketNumber -> (Buckets, Int) -> State
finalState startBucket (buckets, moves) =
    let
        firstBucket =
            Tuple.first buckets |> Tuple.first
        secondBucket =
            Tuple.second buckets |> Tuple.first
        (bucketOne, bucketTwo) =
            case startBucket of
                One -> (firstBucket, secondBucket)
                Two -> (secondBucket, firstBucket)
    in
        { moves = moves, bucketOne = bucketOne, bucketTwo = bucketTwo }

-- fill fills the first bucket to capacity.
fill : Buckets -> Buckets
fill ((_, capacity), secondBucket) =
    ((capacity, capacity), secondBucket)

-- empty empties the first bucket.
empty : Buckets -> Buckets
empty ((_, capacity), secondBucket) =
    ((0, capacity), secondBucket)

-- pour pours as much liquid as possible from the first to the second bucket.
pour : Buckets -> Buckets
pour ((firstSize, firstCapacity), (secondSize, secondCapacity)) =
    let
        poured =
            min firstSize (secondCapacity - secondSize)
    in
        ((firstSize - poured, firstCapacity), (secondSize + poured, secondCapacity))

-- invertBuckets swaps the first and the second buckets.
invertBuckets : Buckets -> Buckets
invertBuckets (bucket1, bucket2) =
    (bucket2, bucket1)

-- Given an operation that works on the first bucket (or from the first to the second),
-- returns a function that performs the same operation in reverse, e.g. the operation
-- works on the second bucket (or from the second to the first).
reverseOp : (Buckets -> Buckets) -> Buckets -> Buckets
reverseOp op =
    invertBuckets >> op >> invertBuckets

-- isValid determines if the given intermediate state is valid.
-- The only invalid state is if the starting bucket is empty and the other bucket is full.
isValid : Buckets -> Bool
isValid ((b1, _), (b2, size)) =
    b1 /= 0 || b2 < size

-- goalReached checks if the goal has been reached for an intermediate state.
goalReached : Int -> Buckets -> Bool
goalReached goal ((bucket1, _), (bucket2, _)) =
    (bucket1 == goal) || (bucket2 == goal)

-- nextBuckets returns all possible next intermediate states in the problem solving
-- by applying all possible actions to the current state. Does not return invalid states.
nextBuckets : Buckets -> List Buckets
nextBuckets buckets =
    [fill, empty, pour]
        |> List.concatMap (\op -> [op, reverseOp op])
        |> List.map (\op -> op buckets)
        |> List.filter isValid
        |> List.filter (\b -> b /= buckets)

-- tailrecMeasure attempts to solve the problem recursively using BFS.
-- If the problem has a solution, `Just` that solution is returned with the
-- number of moves required to reach it; otherwise, `Nothing` is returned.
--
-- seen is a set of intermediate states already seen, to avoid infinite looping
-- if the problem has no solution. Pass an empty set on first call.
tailrecMeasure : Int -> Int -> Set Buckets -> Set Buckets -> Maybe (Buckets, Int)
tailrecMeasure goal moves seen buckets =
    let
        validBuckets =
            Set.diff buckets seen
        goalBucket =
            Set.filter (goalReached goal) validBuckets
                |> Set.toList
                |> List.head
    in
        if Set.isEmpty validBuckets then
            Nothing
        else case goalBucket of
            Just gg -> Just (gg, moves)
            Nothing -> Set.toList validBuckets
                |> List.concatMap nextBuckets
                |> Set.fromList
                |> tailrecMeasure goal (moves + 1) (Set.union seen validBuckets)
