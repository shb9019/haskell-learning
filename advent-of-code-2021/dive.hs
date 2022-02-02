import System.IO

getDirections :: [String] -> [String]
getDirections xs = [x | (x, i) <- zip xs [0..], (i `mod` 2 == 0)]

getValues :: [String] -> [Integer]
getValues xs = [read x :: Integer | (x, i) <- zip xs [0..], (i `mod` 2 == 1)]

getForward :: [String] -> Integer
getForward xs = sum [x | let directions = getDirections xs, (x, i) <- zip (getValues xs) [0..], directions !! i == "forward"] 

getDepthDiff :: Integer -> String -> Integer
getDepthDiff value "up" = -value
getDepthDiff value "down" = value

getDepth :: [String] -> Integer
getDepth xs = sum [(getDepthDiff x (directions !! i)) | let directions = getDirections xs, (x, i) <- zip (getValues xs) [0..], (directions !! i == "down") || (directions !! i == "up")] 

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
    -- let input = map read singlewords :: [String]
    print ((getDepth singlewords) * (getForward singlewords))
    hClose handle
