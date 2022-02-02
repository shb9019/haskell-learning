import System.IO
import Data.List

split :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n xs = (take n xs) : (groupsOf n (drop n xs))

characterCounts :: [String] -> [(Char, Int)]
characterCounts = map(\x -> (head x, length x)) . group . sort . concat

getFromMaybe :: Maybe (Char, Int) -> (Char, Int)
getFromMaybe Nothing = ('0', 0)
getFromMaybe (Just x) = x

getDigitFromIntArray :: [Int] -> Int
getDigitFromIntArray [1,4] = 1
getDigitFromIntArray [0,1,3,5,6] = 2
getDigitFromIntArray [0,1,3,4,5] = 3
getDigitFromIntArray [1,2,3,4] = 4
getDigitFromIntArray [0,2,3,4,5] = 5
getDigitFromIntArray [0,2,3,4,5,6] = 6
getDigitFromIntArray [0,1,4] = 7
getDigitFromIntArray [0,1,2,3,4,5,6] = 8
getDigitFromIntArray [0,1,2,3,4,5] = 9
getDigitFromIntArray [0,1,2,4,5,6] = 0

getMapping :: [String] -> [Char]
getMapping input =
    let
        charCounts = characterCounts input
        result = take 7 $ repeat '0'
        gPosition = getFromMaybe $ find (\x -> snd x == 4) charCounts
        result1 = (take 6 result) ++ [(fst gPosition)]
        cPosition = getFromMaybe $ find (\x -> snd x == 6) charCounts
        result2 = (take 2 result1) ++ [(fst cPosition)] ++ (drop 3 result1)
        ePosition = getFromMaybe $ find (\x -> snd x == 9) charCounts
        result3 = (take 4 result2) ++ [(fst ePosition)] ++ (drop 5 result2)

        positionsWith7 = map fst $ filter (\x -> snd x == 7) charCounts
        fourDigit = filter (\x -> length x == 4) input !! 0
        dPosition = case (find (\x -> x `elem` fourDigit) positionsWith7) of
            Just a -> a
            Nothing -> '0'
        result4 = (take 3 result3) ++ [dPosition] ++ (drop 4 result3)
        fPosition = case (find (\x -> not (x `elem` fourDigit)) positionsWith7) of
            Just a -> a
            Nothing -> '0'
        result5 = (take 5 result4) ++ [fPosition] ++ (drop 6 result4)

        positionsWith8 = map fst $ filter (\x -> snd x == 8) charCounts
        oneDigit = filter (\x -> length x == 2) input !! 0
        bPosition = case (find (\x -> x `elem` oneDigit) positionsWith8) of
            Just a -> a
            Nothing -> '0'
        result6 = (take 1 result5) ++ [bPosition] ++ (drop 2 result5)
        aPosition = case (find (\x -> not (x `elem` oneDigit)) positionsWith8) of
            Just a -> a
            Nothing -> '0'
        result7 = aPosition:(drop 1 result6)
    in
        result7

getValue :: String -> String -> Int
getValue _ [] = 0
getValue mapping value = getDigitFromIntArray $ map snd $ filter (\(x,_) -> x `elem` value) $ zip mapping [0..6]

getAnswer :: String -> [String] -> [Int]
getAnswer mapping values = foldr (\x acc -> (getValue mapping x):acc) [] values

getAnswers :: [String] -> [[String]] -> [[Int]]
getAnswers [] valuesArrays = []
getAnswers mappings valuesArrays = (getAnswer (head mappings) $ head valuesArrays):(getAnswers (tail mappings) $ tail valuesArrays)

convertToNum :: [Int] -> Int
convertToNum [] = 0
convertToNum digits = foldl (\acc x -> (10*acc + x)) 0 digits

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let
        input = split 15 $ words contents
        digits = map (\x -> take 10 x) input
        mappings = map getMapping digits
        values = map (\x -> drop 11 x) input
    print $ sum $ map convertToNum $ getAnswers mappings values
