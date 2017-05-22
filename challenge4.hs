module Main where

import System.IO (readFile, writeFile)
import Data.List (subsequences, sort, takeWhile)
import Data.Maybe (catMaybes)

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

sublists :: Int -> [a] -> [[a]]
sublists  0 _ = [[]]
sublists  _ [] = []
sublists n (x:xs) = map (x:) (sublists (n-1) xs) ++ sublists n xs

getTriangles :: [Integer] -> [Maybe Triangle]
getTriangles [] = []
getTriangles (x:xs) = (smallestTriangleWithX x xs) : getTriangles xs

smallestTriangleWithX :: Integer -> [Integer] -> Maybe Triangle
smallestTriangleWithX a xs = head'
                           . filter validTriangle
                           . map (\(x:y:z:[]) -> (x,y,z))
                           . map (a:) 
                           . sublists 2 $ xs

minTriangle :: [Triangle] -> Maybe Integer
minTriangle [] = Nothing
minTriangle xs = Just $ (minimum . map (\(a,b,c) -> a+b+c)) xs

head' :: [Triangle] -> Maybe Triangle
head' [] = Nothing
head' (x:_) = Just x

minimum' :: [Triangle] -> Maybe Triangle
minimum' [] = Nothing
minimum' xs = Just (minimum xs)

doWork :: [Integer] -> Maybe Integer
doWork = minTriangle
       . catMaybes
       . getTriangles
       . sort

processInput :: [String] -> [[Integer]]
processInput [] = []
processInput (x:xs) = ((map read . tail . words) x) : processInput xs

challenge :: [String] -> String
challenge = unlines . format . map doWork . processInput

main = do
    contents <- readFile "submit_input4.txt"
    -- contents <- readFile "test_input4.txt"
    let l = tail . lines $ contents

    -- print . challenge $ l
    -- writeFile "test_output4.txt" . challenge $ l
    writeFile "submit_output4.txt" . challenge $ l
