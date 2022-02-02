import System.IO
import Text.Read
import Data.List

getSums :: [Integer] -> [Integer]
getSums arr = [sum [(arr !! i), (arr !! (i - 1)), (arr !! (i - 2))] | i <- [2..(length arr - 1)]]

getAns :: [Integer] -> Integer
getAns arr = sum [1 | let input = (getSums arr), (i, x) <- zip [0..] input, i > 0, x > input !! (i - 1)]

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
    let input = map read singlewords :: [Integer]
    print (getAns input)
    hClose handle
