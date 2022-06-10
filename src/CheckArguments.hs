--
-- EPITECH PROJECT, 2022
-- B-FUN-400-STG-4-1-wolfram-romanie.de-meyer
-- File description:
-- arguments
--

module CheckArguments where

import Error

checkInt :: String -> Bool
checkInt a = case reads a :: [(Integer, String)] of
    [(_, "")] -> True
    _ -> False

checkRule :: String -> IO ()
checkRule str
    | str /= "--rule" && str /= "--start" && str /= "--lines" && str /= "--window" && str /= "--move" = printError "Wrong Arguments."
    | otherwise = return ()

checkArgs :: [String] -> Int -> Int -> IO ()
checkArgs args i lenArgs
    | i == lenArgs = return ()
    | i `mod` 2 == 0 = checkRule (args!!i)
    | checkInt (args!!i) == False = printError "It's not a number."
    | otherwise = checkArgs args (i + 1) lenArgs