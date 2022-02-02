import System.IO
import Data.List
import Data.Function (on)

splitStringByComma :: String -> [String]
splitStringByComma s = words $ map (\c -> if c == ',' then ' '; else c) s

processInput :: String -> [Int]
processInput [] = []
processInput x = map (\p -> read p :: Int) $ splitStringByComma x

cost :: Int -> Int -> Int
cost a b = (diff * (diff + 1)) `div` 2
    where
        diff = abs $ a - b

totalCost :: [Int] -> Int -> Int
totalCost positions target = sum $ map (cost target) positions

findMin :: [Int] -> Int
findMin input = snd $ minimumBy (compare `on` snd) $ map (\x -> (x, totalCost input x)) [(minimum input)..(maximum input)]

main = do
    contents <- readFile "input.txt"
    let fileLines = lines contents
    let input = processInput $ lines !! 0
    print $ findMin input
