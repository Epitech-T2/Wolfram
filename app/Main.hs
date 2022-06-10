--
-- EPITECH PROJECT, 2022
-- B-FUN-400-STG-4-1-wolfram-romanie.de-meyer
-- File description:
-- main
--

import System.Environment
import System.Exit
import Data.Char

import CheckArguments
import GetArguments
import Error

main :: IO ()
main = do
    args <- getArgs
    if (length args) < 2
        then printError "No Arguments."
    else checkArgs args 0 (length args)
    getArguments args (length args)
    exitWith ExitSuccess