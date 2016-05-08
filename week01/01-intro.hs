---------------------
-- # 01-intro.hs # --
---------------------

import Prelude hiding (reverse, sum)

-- | reverses a list
reverse :: [a] -> [a]
reverse xs = helper [] xs
  where
    helper ys [] = ys
    helper ys (x : xs) = helper (x : ys) xs

-- | sums a list
sum :: Num a => [a] -> a
sum xs = helper 0 xs
  where
    helper acc [] = acc
    helper acc (x : xs) = helper (acc + x) xs

----------------------
-- ## Exercise 1 ## --
----------------------

-- | converts an integer to the list of its digits
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (read . pure) $ show n -- this is cheating <.<;

-- converts an integer to the reverse of the list of its digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

----------------------
-- ## Exercise 2 ## --
----------------------

-- | doubles the second to last, fourth to last, ...
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . helper . reverse
  where
    helper [] = []
    helper (x : []) = x : []
    helper (x : y : ys) = x : 2 * y : helper ys

----------------------
-- ## Exercise 3 ## --
----------------------

-- | sums the digits of a list (of integers)
sumDigits :: [Integer] -> Integer
sumDigits = sum . map sum . map toDigits

----------------------
-- ## Exercise 4 ## --
----------------------

-- | validates a credit card number
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0

----------------------
-- ## Exercise 5 ## --
----------------------

type Peg = String
type Move = (Peg, Peg)

-- | solves Tower of Hanoi puzzle
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) c b a ++ (a, b) : hanoi (n - 1) a c b
