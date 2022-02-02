import System.IO
import Data.List

getAllCoordinates :: Int -> Int -> [(Int, Int)]
getAllCoordinates n m = [(x,y) | x <- [0..(n-1)], y <- [0..(m-1)]]

updateMap :: [[Char]] -> (Int, Int) -> [[Char]]
updateMap map (x,y) =
    let
        row = (map !! x)
        row1 = ((take y row) ++ ['-'] ++ (drop (y+1) row))
        map1 = ((take x map) ++ [row1] ++ (drop (x+1) map))
    in
        map1

isInside :: (Int, Int) -> Int -> Int -> Bool
isInside (x,y) n m = (x >= 0) && (x < n) && (y >= 0) && (y < m)

countDashes :: [String] -> Int -> Int -> (Int, Int) -> Int
countDashes map n m (x,y)
    | (x == n-1 && y == m-1) = if (((map !! x) !! y) == '-') then 1 else 0
    | y == m-1 = (countDashes map n m (x+1,0)) + (if (((map !! x) !! y) == '-') then 1 else 0)
    | otherwise = (countDashes map n m (x,y+1)) + (if (((map !! x) !! y) == '-') then 1 else 0)

explore :: [[Char]] -> (Int, Int) -> ([String], Int)
explore map (x,y) =
    let
        n = length map
        m = length (map !! 0)
        shouldExplore = not ((((map !! x) !! y) == '9') || (((map !! x) !! y) == '-'))
        (map0, ans0) = if (shouldExplore) then (updateMap map (x,y), 1) else (map, 0)
        (map1, ans1) = if (shouldExplore && (isInside (x+1,y) n m)) then explore map0 (x+1,y) else (map0, 0)
        (map2, ans2) = if (shouldExplore && (isInside (x,y+1) n m)) then explore map1 (x,y+1) else (map1, 0)
        (map3, ans3) = if (shouldExplore && (isInside (x-1,y) n m)) then explore map2 (x-1,y) else (map2, 0)
        (map4, ans4) = if (shouldExplore && (isInside (x,y-1) n m)) then explore map3 (x,y-1) else (map3, 0)
    in
        (map4, ans0 + ans1 + ans2 + ans3 + ans4)

exploreEverything :: [String] -> [(Int,Int)] -> [Int]
exploreEverything map [] = []
exploreEverything map coordinates =
    let
        n = length map
        m = length (map !! 0)
        pos@(x,y) = head coordinates
        posValue = ((map !! x) !! y)
        shouldExplore = not ((posValue == '9') || (posValue == '-'))
        (map1, ans1) = if (shouldExplore) then (explore map pos) else (map, 0)
        rem = exploreEverything map1 (tail coordinates)
    in
        if (ans1 /= 0) then (ans1:rem) else rem

getAnswer :: [String] -> Int
getAnswer map =
    let
        coordinates = getAllCoordinates (length map) (length $ map !! 0)
        basins = exploreEverything map coordinates
        numBasins = length basins
    in
        product . (take 3) . reverse . sort $ basins

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let
        map = words contents
    print $ getAnswer map
