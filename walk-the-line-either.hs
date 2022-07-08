import Control.Monad.Error

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Either String Pole
landLeft n (left, right)
    | abs ((left + n) - right) < 4 = Right (left + n, right)
    | otherwise                    = Left $ "Failed, Left = " ++ (show (left + n)) ++ ", Right = " ++ (show right)

landRight :: Birds -> Pole -> Either String Pole
landRight n (left, right)
    | abs ((right + n) - left) < 4 = Right (left, right + n)
    | otherwise                    = Left $ "Failed, Left = " ++ (show left) ++ ", Right = " ++ (show $ right + n)

banana :: Pole -> Either String Pole
banana _ = Left "Slipped on a banana"

x -: f = f x
