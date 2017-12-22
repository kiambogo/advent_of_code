--- Day 1: Inverse Captcha ---

-- The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.

-- For example:
-- 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
-- 1111 produces 4 because each digit (all 1) matches the next.
-- 1234 produces 0 because no digit matches the next.
-- 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.

import System.IO

main :: IO ()
main = do
  inputStr <- readFile "input"
  let list = map (read . (:"")) (stripNewLine inputStr) :: [Int]
      in putStrLn $ show $ sumMatching list

sumMatching :: (Eq a, Num a) => [a] -> a
sumMatching [] = 0
sumMatching xs = sum [x | (x, y) <- pairs xs, x == y]

-- Generates pairs to compare, including the start and end elements since it's a circular list
pairs :: (Eq a, Num a) => [a] -> [(a, a)]
pairs xs = zip xs $ drop 1 $ xs ++ (take 1 xs)

stripNewLine :: String -> String
stripNewLine = filter (/= '\n')
