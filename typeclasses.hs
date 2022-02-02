-- # Class is for defining typeclasses. More like interfaces in imperative programming.
-- # The exact commented code is declared in Prelude.
-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
-- # This recursive definition allows us to only define one of them in the instances.
--     x == y = not (x /= y)
--     x /= y = not (x == y)

data TrafficLight = Red | Yellow | Green

-- Makes TrafficLight an instance of the typeclass Eq.
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"
