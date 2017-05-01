module Main where

import System.IO (readFile, writeFile)

challenge1 :: [String] -> [Int]
challenge1 [] = []
challenge1 (_:x:xs) = (countPizzas . map read . words) x : challenge1 xs
    where
        countPizzas xs = ceiling ( sum xs / 8)

format :: [Int] -> [String]
format xs = map (\(i,x) -> "Case #" ++ show i ++ ": " ++ show x) $ zip [1..] xs

main = do
    contents <- readFile "submit_input1.txt"
    let l = lines contents
        l' = tail l
    writeFile "submit_output1.txt" . unlines . format . challenge1 $ l'
