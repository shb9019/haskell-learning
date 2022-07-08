import System.Random
import Data.List (nub, groupBy, sortBy)
import Data.Function (on)

type TileValue = Int
data TileColor = Red | Blue | Black | Yellow deriving (Enum, Show, Eq, Ord)
type Tile = (TileValue, TileColor)
type Tray = [Tile]
type Set = [Tile]

-- Returns one random tile
getRandomTile :: RandomGen g => g -> (Tile, g)
getRandomTile gen = do
    let (randomTileValue, gen1)  = randomR (1, 13) gen
    let (randomTileColor, gen2) = randomR (0, 3) gen1
    ((randomTileValue, toEnum randomTileColor), gen2)

-- Returns infinite stream of random tiles
getRandomTileInf :: RandomGen g => g -> [Tile]
getRandomTileInf gen = (randomTile:(getRandomTileInf gen1))
    where (randomTile, gen1) = getRandomTile gen

groupConsecutive :: Set -> [Set]
groupConsecutive = foldl foldingFunction [[]]
    where foldingFunction ([]:xs) y = ([y]:xs)
          foldingFunction (x:xs) y  = if ((fst y) == (fst $ x !! 0)+1) then (y:x):xs else ([[y]] ++ x:xs)

-- Function #1: Returns 14 random unique tiles
getRandomTray :: RandomGen g => g -> Tray
getRandomTray g = take 14 $ nub $ getRandomTileInf g

-- Function #2.1: Given tray, returns all sets containing 3+ tiles with same number
getSetsByValue :: Tray -> [Set]
getSetsByValue = filter (\x -> length x > 2) . groupBy ((==) `on` fst) . sortBy (compare `on` fst)

-- Function #2.2: Given tray, returns all sets containing 3+ tiles of same color with consecutive numbers
getSetsByRun :: Tray -> [Set]
getSetsByRun tray = do
  let colorGroups = groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ tray
  let colorGroupsSorted = map (sortBy (compare `on` fst)) colorGroups
  filter (\x -> length x > 2) $ concat $ map groupConsecutive colorGroupsSorted

-- Function #2: Given tray, returns all valid sets. (Note: There can be possible duplicates)
getSets :: Tray -> [Set]
getSets tray = getSetsByValue tray ++ getSetsByRun tray
