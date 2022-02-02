import System.IO

asInt :: String -> Int
asInt s = read s :: Int

splitStringByComma :: String -> [String]
splitStringByComma s = words (map (\c -> if c == ',' then ' '; else c) s)

getMoves :: String -> [Int]
getMoves s = map asInt (splitStringByComma s)

getNumBoards :: Int -> Int
getNumBoards n = (n `div` 25)

getPlayingBoards :: [String] -> [[Int]]
getPlayingBoards s = [[(asInt x) | (x, j) <- zip s [0..], ((j `div` 25) == i)] | i <- [0..(getNumBoards (length s) - 1)]]

splitBoardByRow :: [Int] -> [[Int]]
splitBoardByRow s = [[(s !! (i * 5 + j)) | j <- [0..4]] | i <- [0..4]]

splitBoardByColumn :: [Int] -> [[Int]]
splitBoardByColumn s = [[(s !! (j * 5 + i)) | j <- [0..4]] | i <- [0..4]]

isRowComplete :: [Int] -> Bool
isRowComplete s = or (map (\row -> (length (filter (== -1) row)) == (length row)) (splitBoardByRow s))

isColumnComplete :: [Int] -> Bool
isColumnComplete s = or (map (\col -> (length (filter (== -1) col)) == (length col)) (splitBoardByColumn s))

isAnyComplete :: [Int] -> Bool
isAnyComplete s = or [isRowComplete s, isColumnComplete s]

getWinningBoard :: [[Int]] -> [Int]
getWinningBoard [] = []
getWinningBoard s = if isAnyComplete (head s) then (head s) else getWinningBoard (tail s)

getWinningScore :: [Int] -> Int -> Int
getWinningScore board val = val * (sum [x | x <- board, x /= -1])

markValueInBoards :: [[Int]] -> Int -> [[Int]]
markValueInBoards boards val = map (\board -> map (\x -> if x == val then -1 else x) board) boards

getAnswer :: [[Int]] -> [Int] -> Int
getAnswer boards [] = 0
getAnswer boards moves =
    let
        markedBoards = markValueInBoards boards (head moves)
        winningBoard = getWinningBoard markedBoards
        isWinning = winningBoard /= []
    in
        if isWinning then getWinningScore winningBoard (head moves) else getAnswer markedBoards (tail moves)


main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let input = words contents
    let moves = getMoves (input !! 0)
    let playingBoards = getPlayingBoards (tail input)
    print (getAnswer playingBoards moves)
    hClose handle
