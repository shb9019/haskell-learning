import System.IO
import System.Environment
import Control.Monad

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x,"")] -> Just x
                                _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x:y:xs) "*"       = return ((x * y):xs)
foldingFunction (x:y:xs) "+"       = return ((x + y):xs)
foldingFunction (x:y:xs) "-"       = return ((y - x):xs)
foldingFunction (x:y:xs) "/"       = return ((y / x):xs)
foldingFunction (x:y:xs) "^"       = return ((y ** x):xs)
foldingFunction (x:xs)   "ln"      = return (log x:xs)
foldingFunction xs       "sum"     = return ([sum xs])
foldingFunction xs       numString = liftM (:xs) $ readMaybe numString

solveRPN :: String -> Maybe Double
solveRPN st = do
    [result] <- foldM foldingFunction [] (words st)
    return result

-- main = do
--     (expression:_) <- getArgs
--     putStrLn $ "Answer: " ++ (show $ solveRPN expression)
