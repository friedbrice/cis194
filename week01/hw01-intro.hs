-----------------------
-- # hw01-intro.hs # --
-----------------------

{-# OPTIONS_GHC -Wall #-}
import Prelude hiding (reverse, sum)

-- | @reverse xs@ returns the reverse of list @xs@
reverse :: [a] -> [a]
reverse xs = helper [] xs
  where
    helper ys [] = ys
    helper ys (x : xs) = helper (x : ys) xs

-- | @sum xs@ returns the sum of list @xs@.
sum :: Num a => [a] -> a
sum xs = helper 0 xs
  where
    helper acc [] = acc
    helper acc (x : xs) = helper (acc + x) xs

----------------------
-- ## Exercise 1 ## --
----------------------

-- | @toDigits n@ converts integer @n@ to the list of its digits.
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = map (read . pure) $ show n

-- @toDigitsRev n@ converts integer @n@ to the reverse list of its digits.
toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

----------------------
-- ## Exercise 2 ## --
----------------------

-- | @doubleEveryOther ns@ doubles the second to last, fourth to last, ...
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . helper . reverse
  where
    helper [] = []
    helper (x : []) = x : []
    helper (x : y : ys) = x : 2 * y : helper ys

----------------------
-- ## Exercise 3 ## --
----------------------

-- | @sumDigits ns@ sums the digits of a list (of integers).
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

----------------------
-- ## Exercise 4 ## --
----------------------

-- | @validate n@ checks to see if @n@ is a valid credit card number.
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0

----------------------
-- ## Exercise 5 ## --
----------------------

type Peg = String
type Move = (Peg, Peg)

-- | @hanoi n a b c@ solves Tower of Hanoi puzzle.
hanoi
  :: Integer -- ^ integer @n@, the number of discs
  -> Peg -- ^ peg @a@, the starting peg
  -> Peg -- ^ peg @b@, the destination peg
  -> Peg -- ^ peg @c@, the temp storage peg
  -> [Move] -- ^ the list of moves, starting from the right
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) c b a ++ (a, b) : hanoi (n - 1) a c b
