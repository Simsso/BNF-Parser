{-# OPTIONS_GHC -Wall #-}

module Parser.Util where

import Text.Megaparsec
import Text.Megaparsec.String
  
c2s :: Char -> String
c2s = (:[])

cp2s :: Parser Char -> Parser String
cp2s = fmap (:[])

lt :: Parser Char
lt = char '<'

gt :: Parser Char
gt = char '>'

sq :: Parser Char
sq = char '\''

dq :: Parser Char
dq = char '"'
