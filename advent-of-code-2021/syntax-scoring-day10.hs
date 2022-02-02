import System.Environment
import System.IO
import Data.List (sort)

getScore :: Char -> Int
getScore '(' = 1
getScore '[' = 2
getScore '{' = 3
getScore '<' = 4
getScore _ = 0

areMatching :: Char -> Char -> Bool
areMatching '(' ')' = True
areMatching '[' ']' = True
areMatching '{' '}' = True
areMatching '<' '>' = True
areMatching _ _     = False

isOpeningItem x = (/=) (getScore x) 0

getStringScore :: String -> Int
getStringScore = foldl foldfunc 0 where foldfunc acc = (+) (5 * acc) . getScore

getMaybeArrayScores :: [Maybe String] -> [Int]
getMaybeArrayScores = map getScore
    where getScore item =
            case item of
                Nothing  -> 0
                Just arr -> getStringScore arr

getMaybeArrayScore :: [Maybe String] -> Int
getMaybeArrayScore = getMiddleItem . sort . (filter (/= 0)) . getMaybeArrayScores
    where getMiddleItem list = (list !! (length list `div` 2))

getPendingItems :: String -> Maybe String
getPendingItems = foldl foldfunc (Just [])
    where foldfunc stack item =
            case stack of
                Nothing  -> Nothing
                Just []  -> case (isOpeningItem item) of
                                True  -> Just [item]
                                False -> Nothing
                Just arr -> case (isOpeningItem item) of
                                True  -> Just (item:arr)
                                False -> case (areMatching (head arr) item) of
                                            True  -> (Just (tail arr))
                                            False -> Nothing

main = do
    (fileName:_) <- getArgs
    contents <- readFile "input.txt"
    let
        input = lines contents
    print $ getMaybeArrayScore  $ map getPendingItems input
