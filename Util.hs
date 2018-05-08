{-# OPTIONS_GHC -Wall #-}

module Util where
  
import Text.Trifecta

runParser :: Parser a -> String -> Bool
runParser p s = go $ parseString p mempty s where
  go (Success _) = True
  go _           = False
  
c2s :: Char -> String
c2s = (:[])

cp2s :: Parser Char -> Parser String
cp2s = fmap (:[])

lt :: CharParsing m => m Char
lt = char '<'

gt :: CharParsing m => m Char
gt = char '>'

sq :: CharParsing m => m Char
sq = char '\''

dq :: CharParsing m => m Char
dq = char '"'