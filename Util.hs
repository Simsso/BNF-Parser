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