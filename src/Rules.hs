--
-- EPITECH PROJECT, 2022
-- B-FUN-400-STG-4-1-wolfram-romanie.de-meyer
-- File description:
-- Rules
--

module Rules where

import System.Environment
import System.Exit
import Data.Char

rule30 :: String -> String
rule30 str
    | str == "*  " = "*"
    | str == " **" = "*"
    | str == " * " = "*"
    | str == "  *" = "*"
    | otherwise = " "

rule90 :: String -> String
rule90 "** " = "*"
rule90 _ = " "
rule90 str
    | str == "** " = "*"
    | str == "*  " = "*"
    | str == " **" = "*"
    | str == "  *" = "*"
    | otherwise = " "

rule110 :: String -> String
rule110 str
    | str == "** " = "*"
    | str == "* *" = "*"
    | str == " **" = "*"
    | str == " * " = "*"
    | str == "  *" = "*"
    | otherwise = " "
