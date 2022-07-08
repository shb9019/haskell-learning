import Control.Monad
import Data.Ratio
import Data.List (all)

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs, p) = map (\(x,r) -> (x,p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x,1%1)]
    m >>= f = flatten (fmap f m)

instance Applicative Prob where
  pure  = return
  (<*>) = ap

data Coin = Heads | Tails deriving (Show,Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return (all (==Tails) [a,b,c])

combinePr :: Prob Bool -> Prob Bool
combinePr (Prob xs) = Prob $ foldr mappingFunction [(False, 0%1),(True, 0%1)] xs
    where mappingFunction (True, p) [(False,x1),(True,x2)] = [(False,x1),(True,p+x2)]
          mappingFunction (False, p) [(False,x1),(True,x2)] = [(False,p+x1),(True,x2)]
