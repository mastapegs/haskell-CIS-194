module CourseOne
  ( toDigits,
    toDigitsRev,
    doubleEveryOther,
    validate,
  )
where

toDigits :: Integer -> [Integer]
toDigits number =
  if number <= 0
    then []
    else go number []
  where
    go :: Integer -> [Integer] -> [Integer]
    go 0 result = result
    go number result = go (number `div` 10) (mod number 10 : result)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther list = go (reverse list) False []
  where
    go :: [Integer] -> Bool -> [Integer] -> [Integer]
    go [] _ result = result
    go (x : xs) True result = go xs False ((2 * x) : result)
    go (x : xs) False result = go xs True (x : result)

sumDigits :: [Integer] -> Integer
sumDigits list = sum (map (sum . toDigits) list)

validate :: Integer -> Bool
validate number = sumDigits ((doubleEveryOther . toDigits) number) `mod` 10 == 0

-- To move n discs (stacked in increasing size) from peg a to peg b
-- using peg c as temporary storage,
-- 1. move n − 1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n − 1 discs from c to b using a as temporary storage.

type Peg = String

type Move = (Peg, Peg)

-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi discs p1 p2 p3 = hanoiHelper discs p1 p2 p3 []
  where
    hanoiHelper :: Integer -> Peg -> Peg -> Peg -> [Move] -> [Move]
    hanoiHelper discs p1 p2 p3 moves = undefined