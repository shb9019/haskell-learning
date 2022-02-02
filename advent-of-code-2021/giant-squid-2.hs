import System.IO
import Data.List

asInt :: String -> Int
asInt s = read s :: Int

splitStringByComma :: String -> [String]
splitStringByComma s = words $ map (\c -> if c == ',' then ' '; else c) s

getMoves :: String -> [Int]
getMoves s = map asInt $ splitStringByComma s

getBoards :: [String] -> [[Int]]
getBoards [] = []
getBoards xs =
    let
        (board, rest) = splitAt 25 xs
    in
        (map asInt board) : getBoards rest

splitBoard :: [Int] -> [[Int]]
splitBoard [] = []
splitBoard xs =
    let
        (board, rest) = splitAt 5 xs
    in
        board : splitBoard rest

isComplete :: [Int] -> Bool
isComplete board = 
    let
        rows = splitBoard board
        isRowValid = any (==5) $ map (\row -> (length $ filter (==(-1)) row)) rows
        columns = transpose rows
        isColumnValid = any (==5) $ map (\column -> (length $ filter (==(-1)) column)) columns
    in
        isRowValid || isColumnValid

markValueInBoard :: [Int] -> Int -> [Int]
markValueInBoard board value  = map (\x -> if x == value then -1 else x) board

getBoardScore :: [Int] -> Int -> Int
getBoardScore board currentValue = currentValue * (sum $ map (\x -> if x == -1 then 0 else x) board)

getWinnerScores :: [[Int]] -> [Int] -> [Int]
getWinnerScores _ [] = []
getWinnerScores [] _ = []
getWinnerScores boards moves =
    let
        previousWinners = findIndices isComplete boards
        currentMove = head moves
        updatedBoards = map (\board -> markValueInBoard board currentMove) boards
        winners = (findIndices isComplete updatedBoards) \\ previousWinners
        hasWinner = not $ null winners
        remainingWinnerScores = getWinnerScores updatedBoards (tail moves)
        winnerScores = foldr (\x acc -> (getBoardScore (updatedBoards !! x) currentMove) : acc) [] winners
    in
        if hasWinner then winnerScores ++ remainingWinnerScores else remainingWinnerScores

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents
    let moves = getMoves $ input !! 0
    let boards = getBoards $ drop 1 input
    let winnerScores = getWinnerScores boards moves
    print $ last winnerScores
    hClose handle
