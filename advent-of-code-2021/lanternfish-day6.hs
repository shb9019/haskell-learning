import System.IO
import Data.Char
import Data.Map as Map hiding (foldl, foldr)

getNumbersFromString :: String -> [Int]
getNumbersFromString [] = []
getNumbersFromString (',':xs) = getNumbersFromString xs
getNumbersFromString (x:xs)   = digitToInt x : getNumbersFromString xs

getValueFromMap :: Int -> Map.Map Int Int -> Int
getValueFromMap key = Map.findWithDefault 0 key

getInitialState :: [Int] -> Map.Map Int Int
getInitialState [] = Map.empty
getInitialState (x:xs) = Map.insertWith (+) x 1 (getInitialState xs)

updateState :: Map.Map Int Int -> Map.Map Int Int
updateState state =
    let
        stateValue key = getValueFromMap key state
        map1 = foldl (\acc i -> Map.insert (i-1) (stateValue i) acc) Map.empty [1..8]
        map2 = Map.insert 8 (stateValue 0) map1
        endState = Map.insert 6 ((getValueFromMap 6 map2) + (stateValue 0)) map2
    in
        endState

process :: Map.Map Int Int -> Int -> Map.Map Int Int
process state numDays = foldl (\state _ -> updateState state) state $ [1..numDays]

sumState :: Map.Map Int Int -> Int
sumState state = sum $ foldr (\x acc -> (getValueFromMap x state):acc) [] [0..8]

main = do
    contents <- readFile "input.txt"
    let firstLine = lines contents !! 0
    let input = getNumbersFromString $ firstLine
    let initialState = getInitialState input
    print $ sumState $ process initialState 256
