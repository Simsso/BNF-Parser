{-# OPTIONS_GHC -Wall #-}

module Parser.BNF.Constants where

availableSpecialChars :: [Char]
availableSpecialChars = "| !#$%&()*+,-./:;>=<?@[\\]^_`{}~"

availableLetters :: [Char]
availableLetters = ['a'..'z'] ++ ['A'..'Z']

availableDigits :: [Char]
availableDigits = ['0'..'9']