import Data.List (nub, sort)
import qualified Data.Map as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
