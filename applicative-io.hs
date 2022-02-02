sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

main = do
    a <- (++) <$> getLine <*> getLine
    putStrLn $ "The two lines concatenated turn out too be: " ++ a
