import System.IO

getTails :: [[a]] -> [[a]]
getTails xs = map tail xs

getHeads :: [[a]] -> [a]
getHeads xs = map head xs

getSums :: [[Char]] -> Int -> [Integer]
getSums xs 0 = []
getSums xs n = sum (map (\x -> read (x:[]) :: Integer) (getHeads xs)) : getSums (getTails xs) (n-1)

getMajority :: Integer -> Integer -> Integer
getMajority x m = if x >= (m `div` 2) then 1 else 0

getMajorityList :: [Integer] -> Integer -> [Integer]
getMajorityList sums m = map (\x -> getMajority x m) sums

getDecimalFromBinary :: [Integer] -> Integer
getDecimalFromBinary [] = 0
getDecimalFromBinary xs = (2 * (getDecimalFromBinary (init xs))) + (last xs)

getInverse :: Integer -> Integer
getInverse x = if x == 0 then 1 else 0

getInverseList :: [Integer] -> [Integer]
getInverseList xs = map (\x -> getInverse x) xs

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
    let n = length (singlewords !! 0)
    let m = length singlewords
    let sums = getSums singlewords n
    let majorityList = getMajorityList sums (fromIntegral m)
    let minorityList = getInverseList majorityList
    print (majorityList)
    print (minorityList)
    hClose handle
