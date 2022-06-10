--
-- EPITECH PROJECT, 2022
-- B-FUN-400-STG-4-1-wolfram-romanie.de-meyer
-- File description:
-- GetArguments
--

module GetArguments where

import Error
import Wolfram

ruleValue :: [String] -> [String] -> Int -> Int -> Int
ruleValue tab args i j
    | j == ((length tab) - 1) && (tab!!j) /= (args!!(i + 1)) = -3
    | (tab!!j) == (args!!(i + 1)) = read (tab!!j)
    | otherwise = ruleValue tab args i (j + 1)

getRule :: [String] -> Int -> Int -> [String] -> Int
getRule args i lenArgs tab
    | i == lenArgs = -1
    | "--rule" == (args!!i) && i == (lenArgs - 1) = -2
    | "--rule" /= (args!!i) && i == (lenArgs - 1) = -1
    | "--rule" == (args!!i) = ruleValue tab args i 0
    | otherwise = getRule args (i + 1) lenArgs tab

getStart :: [String] -> Int -> Int -> Int
getStart args i lenArgs
    | i == lenArgs = 0
    | "--start" == (args!!i) = read (args!!(i + 1))
    | "--start" == (args!!i) && i == (lenArgs - 1) = -1
    | otherwise = getStart args (i + 1) lenArgs

getLines :: [String] -> Int -> Int -> Int
getLines args i lenArgs
    | i == lenArgs = -2
    | "--lines" == (args!!i) = read (args!!(i + 1))
    | "--lines" == (args!!i) && i == (lenArgs - 1) = -1
    | otherwise = getLines args (i + 1) lenArgs

getWindow :: [String] -> Int -> Int -> Int
getWindow args i lenArgs
    | i == lenArgs = 80
    | "--window" == (args!!i) = read (args!!(i + 1))
    | "--window" == (args!!i) && i == (lenArgs - 1) = -1
    | otherwise = getWindow args (i + 1) lenArgs

getMove :: [String] -> Int -> Int -> Int
getMove args i lenArgs
    | i == lenArgs = 0
    | "--move" == (args!!i) = read (args!!(i + 1))
    | "--move" == (args!!i) && i == (lenArgs - 1) = -1
    | otherwise = getMove args (i + 1) lenArgs

getArguments :: [String] -> Int -> IO()
getArguments args lenArgs = do
    let tab = ["30", "90", "110"]
    let rule = getRule args 0 lenArgs tab
    ruleErrors rule
    let start = getStart args 0 lenArgs
    othersErrors start "start"
    let lines = getLines args 0 lenArgs
    othersErrors lines "lines"
    let window = getWindow args 0 lenArgs
    othersErrors window "window"
    let move = getMove args 0 lenArgs
    othersErrors move "move"
    wolfram rule start lines window move "*" (-1) 1 0