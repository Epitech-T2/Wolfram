--
-- EPITECH PROJECT, 2022
-- B-FUN-400-STG-4-1-wolfram-romanie.de-meyer
-- File description:
-- Error
--

module Error where

import System.Environment
import System.Exit
import Data.Char

printError :: String -> IO ()
printError str = putStrLn str >> exitWith (ExitFailure 84)

ruleErrors :: Int -> IO ()
ruleErrors (-1) = printError "No '--rule' detected !"
ruleErrors (-2) = printError "There's no value after '--rule' !"
ruleErrors (-3) = printError "The value after '--rule' must be 30, 90 or 110 !"
ruleErrors rule = putStr ""

othersErrors :: Int -> String -> IO ()
othersErrors (-1) str = printError ("There's no value after '"++str++"' !")
othersErrors i str = putStr ""