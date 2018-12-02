-- Part A
-- To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID containing exactly two of any letter and then separately counting those with exactly three of any letter. You can multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.
--
-- For example, if you see the following box IDs:
--
-- abcdef contains no letters that appear exactly two or three times.
-- bababc contains two a and three b, so it counts for both.
-- abbcde contains two b, but no letter appears exactly three times.
-- abcccd contains three c, but no letter appears exactly two times.
-- aabcdd contains two a and two d, but it only counts once.
-- abcdee contains two e.
-- ababab contains three a and three b, but it only counts once.
-- Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.
--
-- What is the checksum for your list of box IDs?

--- Part Two ---
-- Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.
--
-- The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:
--
-- abcde
-- fghij
-- klmno
-- pqrst
-- fguij
-- axcye
-- wvxyz
-- The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.
--
-- What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)

import Data.List
import Data.Maybe

main :: IO ()
main = do
  vals <- fmap lines $ readFile "input.txt"
  putStrLn $ "Part A Solution: " ++ (show $ countTwo vals * countThree vals)
  putStrLn $ "Part B Solution: " ++ (show $ fromJust $ head $ filter (/= Nothing) [offByOne x y | (i,x) <- zip [1..] vals, y <- drop i vals])

countTwo :: [String] -> Int
countTwo strings = length $ filter (/= False) $ fmap (count 2) strings

countThree :: [String] -> Int
countThree strings = length $ filter (/= False) $ fmap (count 3) strings

count :: Int -> String -> Bool
count i input =
  not $ null $ filter (\x -> length x == i) $ group $ sort input

offByOne :: String -> String -> Maybe String
offByOne a b =
  if (length union) >= (length a - 1) then Just union else Nothing
  where union = [x | (i,x) <- zip [1..] a, (j,y) <- zip [1..] b, (i,x)==(j,y)]
