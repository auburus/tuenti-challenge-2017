module Main where

import System.IO (readFile, writeFile)
import Data.List (subsequences)

type Triangle = (Integer, Integer, Integer)
-- type Triangle = [Integer]

validTriangle :: Triangle -> Bool
validTriangle (a,b,c)
    | a + b <= c = False
    | a + c <= b = False
    | b + c <= a = False
    | otherwise = True

format :: [Maybe Integer] -> [String]
format xs = map (\(i,x) -> "Case #" ++ show i ++ ": " ++ show' x) $ zip [1..] xs
    where
        show' = maybe "IMPOSSIBLE" show 

subsequences' :: Int -> [a] -> [[a]]
subsequences' _ [] = []
subsequences' i (x:xs)
    | i == 1 = [[x]] ++ subsequences' i xs
    | otherwise = listsWithX x xs ++ listsWithoutX xs
    where
        listsWithX x xs = map (\s -> (x:s)) . subsequences' (i-1) $ xs
        listsWithoutX xs = subsequences' i xs

sublists :: Int -> [a] -> [[a]]
sublists  0 _ = [[]]
sublists  _ [] = []
sublists n (x:xs) = sublists n xs ++ map (x:) (sublists (n-1) xs)

getTriangles :: [Integer] -> [Triangle]
getTriangles = filter validTriangle
             . map (\(x:y:z:[]) -> (x,y,z))
             . sublists 3

minTriangle :: [Triangle] -> Maybe Integer
minTriangle [] = Nothing
minTriangle xs = Just $ (minimum . map (\(a,b,c) -> a+b+c)) xs

-- TODO CHANGE
processInput :: [String] -> [[Integer]]
processInput [] = []
processInput (x:xs) = ((map read . take 150 . tail . words) x) : processInput xs

challenge :: [String] -> String
challenge = unlines . format . map (minTriangle . getTriangles) . processInput

main = do
    contents <- readFile "submit_input4.txt"
    -- contents <- readFile "test_input4.txt"
    let l = tail . lines $ contents
        
    print . challenge $ l
    -- writeFile "test_output4.txt" . challenge $ l
    -- writeFile "submit_output4.txt" . challenge $ l
