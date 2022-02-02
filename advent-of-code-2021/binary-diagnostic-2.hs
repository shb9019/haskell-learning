import System.IO

getTails :: [[a]] -> [[a]]
getTails xs = map tail xs

getHeads :: [[a]] -> [a]
getHeads xs = map head xs

getIntFromChar :: Char -> Integer
getIntFromChar '0' = 0
getIntFromChar '1' = 1

getArrayOfArrays :: [String] -> [[Integer]]
getArrayOfArrays xs = [x ++ [i] | (x, i) <- zip (map (map getIntFromChar) xs) [0..]]

getMajorityValueByHead :: [[Integer]] -> Integer
getMajorityValueByHead xs =
  let
    heads = getHeads xs
    total = sum heads
  in
    if (fromIntegral total) >= ((fromIntegral (length xs)) / 2) then 1 else 0

getMinorityValueByHead :: [[Integer]] -> Integer
getMinorityValueByHead xs =
  let
    heads = getHeads xs
    total = sum heads
  in
    if (fromIntegral total) >= ((fromIntegral (length xs)) / 2) then 0 else 1


getFilteredByHead :: [[Integer]] -> Integer -> [[Integer]]
getFilteredByHead xs m = [x | x <- xs, (head x) == m]

getDecimalFromBinary :: [Integer] -> Integer
getDecimalFromBinary [] = 0
getDecimalFromBinary xs = (2 * (getDecimalFromBinary (init xs))) + (last xs)

getMajorityMatchSeq :: [[Integer]] -> [[Integer]]
getMajorityMatchSeq xs
    | (length xs) == 1 = xs
    | otherwise = (getMajorityMatchSeq (getTails (getFilteredByHead xs (getMajorityValueByHead xs))))

getMinorityMatchSeq :: [[Integer]] -> [[Integer]]
getMinorityMatchSeq xs
    | (length xs) == 1 = xs
    | otherwise = (getMinorityMatchSeq (getTails (getFilteredByHead xs (getMinorityValueByHead xs))))

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
    let n = length (singlewords !! 0)
    let m = length singlewords
    let arrayOfArrays = getArrayOfArrays singlewords
    let minimumSequence = fromIntegral (last ((getMinorityMatchSeq arrayOfArrays) !! 0))
    let maximumSequence = fromIntegral (last ((getMajorityMatchSeq arrayOfArrays) !! 0))
    let ans1 = getDecimalFromBinary (init (arrayOfArrays !! minimumSequence))
    let ans2 = getDecimalFromBinary (init (arrayOfArrays !! maximumSequence))
    print (ans1 * ans2)
    hClose handle
