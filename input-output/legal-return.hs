main = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    a <- return "hell"
    b <- return "yeah!"
    putStrLn $ a ++ " " ++ b
