import System.IO

getDirections :: [String] -> [String]
getDirections xs = [x | (x, i) <- zip xs [0..], (i `mod` 2 == 0)]

getValues :: [String] -> [Integer]
getValues xs = [read x :: Integer | (x, i) <- zip xs [0..], (i `mod` 2 == 1)]

getAim :: String -> Integer -> Integer
getAim direction value
    | direction == "up" = -value
    | direction == "down" = value
    | otherwise = 0

getAimList :: [String] -> [Integer] -> Integer -> [Integer]
getAimList [] [] prev = []
getAimList directions values prev = concat [x:(getAimList (tail directions) (tail values) x) | let x = (getAim (head directions) (head values)) + prev]

getForward :: [String] -> Integer
getForward xs = sum [x | let directions = getDirections xs, (x, i) <- zip (getValues xs) [0..], directions !! i == "forward"] 

getTotalDepth :: [Integer] -> [Integer] -> [String] -> Integer
getTotalDepth aims values directions = sum [(x * (values !! i)) | (x, i) <- zip aims [0..], directions !! i == "forward"]

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
    let forward = getForward singlewords
    let aimList = (getAimList (getDirections singlewords) (getValues singlewords) 0)
    let totalDepth = (getTotalDepth aimList (getValues singlewords) (getDirections singlewords))
    print (totalDepth * forward)
    hClose handle
