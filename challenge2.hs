module Main where

import System.IO (readFile, writeFile)
import Data.List (intersperse)

score :: [Int] -> [Int]
score [] = []
score s@(x:xs)
    | x == 10 = (strike xs) : score xs
    | otherwise = doScore s
    where
        doScore (x:y:xs)
            | x + y == 10 = (spear xs) : score xs
            | otherwise = (x + y) : score xs

strike [] = 10
strike (x:[]) = 10 + x
strike (x:y:_) = 10 + x + y

spear [] = 10
spear (x:_) = 10 + x

accumScore :: [Int] -> [Int]
accumScore = tail . scanl (+) 0 . take 10 . score 

format :: [[Int]] -> [String]
format xs = map (\(i,x) -> "Case #" ++ show i ++ ": " ++ show' x) $ zip [1..] xs
    where
        -- show' = intersperse ' ' . map show
        show' :: [Int] -> String
        -- show' [] = ""
        -- show' (x:xs) = show x ++ " " ++ show' xs
        show' xs = foldl (++) "" . intersperse " " . map show $ xs

processInput :: [String] -> [[Int]]
processInput [] = []
processInput (_:x:xs) = ((map read . words) x) : processInput xs

challenge2 :: [String] -> String
challenge2 = unlines . format . map accumScore . processInput

main = do
    contents <- readFile "submit_input2.txt"
    -- contents <- readFile "test_input2.txt"
    let l = tail . lines $ contents
        
    print . challenge2 $ l
    -- writeFile "test_output2.txt" . challenge2 $ l
    writeFile "submit_output2.txt" . challenge2 $ l
