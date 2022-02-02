import System.IO

computeAnswer input = sum [1 | (i, x) <- zip [0..] input, i > 0, x > input !! (i - 1)]

main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let singlewords = words contents
    let input = map read singlewords :: [Integer]
    print (computeAnswer input)
    hClose handle

