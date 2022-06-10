--
-- EPITECH PROJECT, 2022
-- B-FUN-400-STG-4-1-wolfram-romanie.de-meyer
-- File description:
-- Wolfram
--

module Wolfram where

import Rules

splitString :: Int -> Int -> String -> String
splitString start end str = take (end - start) (drop start str)

displaySpaces :: String -> Int -> Int -> String
displaySpaces str i window
    | i == window = str
    | otherwise = displaySpaces (str ++ " ") (i + 1) window

charDisplayLineNeg :: String -> Int -> Int -> String
charDisplayLineNeg nextPrint window charDisplayLine
    | length nextPrint <= (-charDisplayLine) = displaySpaces "" 0 window
    | length (splitString (-charDisplayLine) ((-charDisplayLine) + window) nextPrint) < 80 = splitString (-charDisplayLine) ((-charDisplayLine) + window)
        (nextPrint ++ displaySpaces "" 0 (window - length (splitString (-charDisplayLine) ((-charDisplayLine) + window) nextPrint)))
    | otherwise = splitString (-charDisplayLine) ((-charDisplayLine) + window) nextPrint

charDisplayLinePos :: String -> Int -> Int -> String
charDisplayLinePos nextPrint window charDisplayLine
    | charDisplayLine > window = displaySpaces "" 0 window
    | otherwise = displaySpaces "" 0 charDisplayLine ++ splitString 0 (window - charDisplayLine) nextPrint ++
        displaySpaces "" 0 (window - (length (displaySpaces "" 0 charDisplayLine ++
        splitString 0 (window - charDisplayLine) nextPrint)))

displayLine :: String -> Int -> Int -> String
displayLine nextPrint window charDisplayLine
    | charDisplayLine >= 0 = charDisplayLinePos nextPrint window charDisplayLine
    | otherwise = charDisplayLineNeg nextPrint window charDisplayLine

checkChar :: String -> Int -> Char
checkChar firstPrint pos
    | pos < 0 || pos >= length firstPrint = ' '
checkChar firstPrint pos = (firstPrint!!pos)

choiceRule :: Int ->Int -> Int -> String -> Int -> String -> String
choiceRule rule i nbrRow finalPrint pos firstPrint
    | i == nbrRow = finalPrint
    | rule == 30 = choiceRule 30 (i + 1) nbrRow (finalPrint ++ rule30((checkChar firstPrint (pos - 1)) :
        (checkChar firstPrint pos) : (checkChar firstPrint (pos + 1)) : [])) (pos + 1) firstPrint
    | rule == 90 = choiceRule 90 (i + 1) nbrRow (finalPrint ++ rule90((checkChar firstPrint (pos - 1)) :
        (checkChar firstPrint pos) : (checkChar firstPrint (pos + 1)) : [])) (pos + 1) firstPrint
    | rule == 110 = choiceRule 110 (i + 1) nbrRow (finalPrint ++ rule110((checkChar firstPrint (pos - 1)) :
        (checkChar firstPrint pos) : (checkChar firstPrint (pos + 1)) : [])) (pos + 1) firstPrint

wolfram :: Int -> Int -> Int -> Int -> Int -> String -> Int -> Int -> Int ->IO()
wolfram rule start 0 window move firstPrint pos nbrRow i = return ()
wolfram rule start lines window move firstPrint pos nbrRow i
    | i >= start = do
    let nextPrint = (choiceRule rule 0 nbrRow "" (-1) firstPrint)
    let charDisplayLine = ((window `div` 2) - (length nextPrint) `div` 2) + move
    putStrLn (displayLine nextPrint window charDisplayLine)
    wolfram rule start (lines - 1) window move nextPrint (-1) (nbrRow + 2) (i + 1)
wolfram rule start lines window move firstPrint pos nbrRow i = do
    let nextPrint = (choiceRule rule 0 nbrRow "" (-1) firstPrint)
    wolfram rule start lines window move nextPrint (-1) (nbrRow + 2) (i + 1)