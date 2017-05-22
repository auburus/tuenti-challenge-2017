module Main where

import System.IO (readFile, writeFile)
import Data.Array (Array)
import qualified Data.Array as A
import Data.Maybe (maybeToList)
import Data.List (partition)

type Time = Int
type Shortcuts = Array Int [(Int, Time)]
type Tower = Array Int Time

readShortcut :: String -> (Int, (Int, Time))
readShortcut = (\(x:y:z:[]) -> (x, (y, z))) . map read . words

tower :: Int -> Tower
tower floors = A.array (1, floors) defaults
    where
        defaults = (1,0) : (zip [2..floors] . repeat $ (maxBound :: Int))


nextFloor :: Tower -> Int -> Maybe (Int, Time)
nextFloor tower floor
    | floor + 1 > (snd . A.bounds) tower = Nothing
    | otherwise = Just (floor + 1, (tower A.! floor) + floor)

keepShorter :: [(Int, Time)] -> [(Int, Time)]
keepShorter [] = []
keepShorter (x:xs) = shortest : keepShorter xs'
    where
        (common, xs') = partition (\(floor, _) -> floor == fst x) xs
        shortest =
            foldl (\(f1, t1) (f2, t2) -> if t1 < t2 then (f1,t1) else (f2,t2))
                x
                common

shortcuts :: Int -> Shortcuts
shortcuts floors = A.array (1, floors) . zip [1..floors] $ (repeat [])

moveFromFloor :: Tower -> Shortcuts -> Int -> Tower
moveFromFloor tower s floor = tower A.// shorterPaths
    where
        shortcuts = map (\(s, t) -> (s, t + moves)) $ s A.! floor
        moves = tower A.! floor
        paths = (maybeToList (nextFloor tower floor))
              ++ shortcuts
              ++ fakeShortcuts floor shortcuts 
              ++ A.assocs tower
        shorterPaths = keepShorter paths

shortestTimeTower :: Int -> Shortcuts -> Tower
shortestTimeTower floors s =
    foldl (\tower floor -> moveFromFloor tower s floor) 
          (tower floors)
          [low..high]
    where
        (low, high) = (1, floors)

shortestTime :: Tower -> Time
shortestTime t = t A.! last
    where
        (_, last) = A.bounds t

fakeShortcuts :: Int -> [(Int, Time)] -> [(Int, Time)]
fakeShortcuts _ [] = []
fakeShortcuts floor (x:xs) = map (\i -> (i, snd x)) [(floor+1)..(fst x)]
                           ++ fakeShortcuts floor xs

groupShortcuts :: Int -> [(Int, (Int, Time))] -> Array Int [(Int, Time)]
groupShortcuts floors = foldl (\s (i, (dest, t)) -> s A.// [(i, (dest, t) : s A.! i)])
                      . A.listArray (1, floors) 
                      . repeat
                      $ []

parseInput :: String -> [(Int, Shortcuts)]
parseInput = parseCases . tail . lines
    where
        parseCases :: [String] -> [(Int, Shortcuts)]
        parseCases [] = []
        parseCases (x:xs) = (floors, shortcuts) : (parseCases . drop ncases $ xs)
            where
                (floors:ncases:[]) = map read . words $ x :: [Int]
                shortcuts = groupShortcuts floors
                          . map readShortcut
                          . take ncases
                          $ xs

format :: [Int] -> [String]
format xs = map (\(i,x) -> "Case #" ++ show i ++ ": " ++ show x) $ zip [1..] xs

main = do
    -- contents <- readFile "test_input6.txt"
    contents <- readFile "submit_input6.txt"
    let s = A.array (1,5) $ [(1,[(3,0)]), (2, [(5,2)]), (3,[]), (4,[]), (5,[])]
        input = parseInput contents
        shortest = map (\(f, sh) -> shortestTime . shortestTimeTower f $ sh) input
        
    print . unlines . format $ shortest
    -- writeFile "test_output6.txt" . unlines . format $ shortest
    writeFile "submit_output6.txt" . unlines . format $ shortest
    
