module Main where

import System.IO (readFile, writeFile)


minCards :: (Floating b, Integral c, RealFrac b) => b -> c
minCards = ceiling . logBase 2 

format :: [Int] -> [String]
format xs = map (\(i,x) -> "Case #" ++ show i ++ ": " ++ show x) $ zip [1..] xs

processInput :: [String] -> [Float]
processInput [] = []
processInput (x:xs) = (read x) : processInput xs

challenge :: [String] -> String
challenge = unlines . format . map minCards . processInput

main = do
    contents <- readFile "submit_input3.txt"
    -- contents <- readFile "test_input3.txt"
    let l = tail . lines $ contents
        
    print . challenge $ l
    -- writeFile "test_output3.txt" . challenge $ l
    writeFile "submit_output3.txt" . challenge $ l
