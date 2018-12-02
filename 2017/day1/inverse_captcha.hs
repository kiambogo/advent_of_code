--- Day 1: Inverse Captcha ---

-- Part A
--
-- The captcha requires you to review a sequence of digits (your puzzle input) and find the sum of all digits that match the next digit in the list. The list is circular, so the digit after the last digit is the first digit in the list.

-- For example:
-- 1122 produces a sum of 3 (1 + 2) because the first digit (1) matches the second digit and the third digit (2) matches the fourth digit.
-- 1111 produces 4 because each digit (all 1) matches the next.
-- 1234 produces 0 because no digit matches the next.
-- 91212129 produces 9 because the only digit that matches the next one is the last digit, 9.
--
-- Part B
--
-- Now, instead of considering the next digit, it wants you to consider the digit halfway around the circular list. That is, if your list contains 10 items, only include a digit in your sum if the digit 10/2 = 5 steps forward matches it. Fortunately, your list has an even number of elements.

--For example:

--1212 produces 6: the list contains 4 items, and all four digits match the digit 2 items ahead.
--1221 produces 0, because every comparison is between a 1 and a 2.
--123425 produces 4, because both 2s match each other, but no other digit has a match.
--123123 produces 12.
--12131415 produces 4.

main :: IO ()
main = do
  inputStr <- fmap stripNewLine $ readFile "input"
  let list = map (read . (:"")) inputStr :: [Int]
  putStrLn $ "Part A Solution: " ++ (show $ sumMatching 1 list)
  putStrLn $ "Part B Solution: " ++ (show $ sumMatching (length list `div` 2) list)

sumMatching :: (Eq a, Num a) => Int -> [a] -> a
sumMatching _ [] = 0
sumMatching offset xs = sum [x | (x, y) <- pairs offset xs, x == y]

-- Generates pairs to compare, including the start and end elements since it's a circular list
pairs :: (Eq a, Num a) => Int -> [a] -> [(a, a)]
pairs offset xs = zip xs $ drop offset $ xs ++ xs

stripNewLine :: String -> String
stripNewLine = filter (/= '\n')
