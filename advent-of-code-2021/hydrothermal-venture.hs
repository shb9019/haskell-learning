import System.IO
import Data.List

parseCoordinate :: String -> (Int, Int)
parseCoordinate s = (read x :: Int, read y :: Int)
  where
    (x, y1) = span (/= ',') s
    y = tail y1

getLineCoordinates :: [String] -> [((Int, Int), (Int, Int))]
getLineCoordinates [] = []
getLineCoordinates input =
    let
        [p1, _, p2] = take 3 input
        remaining = drop 3 input
    in
        (parseCoordinate p1, parseCoordinate p2) : getLineCoordinates remaining

getOverlappingPoints :: [((Int, Int), (Int, Int))] -> [Int]

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents
    print $ getLineCoordinates input
    hClose handle
