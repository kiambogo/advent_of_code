-- Part A
-- A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3 inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4 inches tall.
-- The problem is that many of the claims overlap, causing two or more claims to cover part of the same areas.
-- The four square inches marked with X are claimed by both 1 and 2. (Claim 3, while adjacent to the others, does not overlap either of them.)

-- If the Elves all proceed with their own plans, none of them will have enough fabric. How many square inches of fabric are within two or more claims?

--- Part Two ---

import qualified Data.Text as T
import qualified Data.Map as M

type Matrix = M.Map Int Int
data Rect = Rect { x :: Int, y :: Int, w :: Int, h :: Int}  deriving Show

main :: IO ()
main = do
  vals <- fmap lines $ readFile "input.txt"
  putStrLn $ "Part A Solution: " ++ (show $ length $ M.filter (>1) $ go baseMatrix $ fmap parseLine vals)
  where baseMatrix = M.fromList $ flatten [[(x+y*1000,0) | x <- [0..999]] | y <- [0..999]]
  -- putStrLn $ "Part B Solution: " ++ (show)

flatten :: [[(Int, Int)]] -> [(Int, Int)]
flatten (a:[]) = a
flatten (a:tail) = a ++ flatten tail

go :: Matrix -> [Rect] -> Matrix
go matrix [] = matrix
go matrix (r:tail) = go (updateMatrix matrix r) tail

updateMatrix :: Matrix -> Rect -> Matrix
updateMatrix matrix rect =
  M.unionWith (+) matrix (M.fromList $ flatten [[(x+y*1000, 1) | x <- [(x rect)+1..(x rect) + (w rect)]] | y <- [(y rect)+1..(y rect) + (h rect)]])

parseLine :: String -> Rect
parseLine line =
  Rect x y w h
  where l = words $ fmap (\x -> if x == ':' then ' ' else x) line
        xy = (T.splitOn (T.pack ",") (T.pack $ l!!2))
        wh = (T.splitOn (T.pack "x") (T.pack $ l!!3))
        x = read $ T.unpack (xy !! 0) :: Int
        y = read $ T.unpack (xy !! 1) :: Int
        w = read $ T.unpack (wh !! 0) :: Int
        h = read $ T.unpack (wh !! 1) :: Int
