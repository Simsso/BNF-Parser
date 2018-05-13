{-# OPTIONS_GHC -Wall #-}

module Parser.BNF.Parsers where

import Control.Applicative
import Parser.BNF.Constants
import Parser.BNF.Types
import Text.Megaparsec
import Text.Megaparsec.String

syntax :: Parser BNFDefinition
syntax = BNFDefinition <$> (rule `sepEndBy1` lineEnd) <* eof

rule :: Parser RuleDefinition
rule = do
  name' <- blanks *> lt *> ruleName <* gt <* blanks
  expr' <- string "::=" *> blanks *> expression
  return $ RuleDefinition name' expr'

expression :: Parser [[Term]]
expression = list `sepBy1` (blanks *> char '|' *> blanks)

lineEnd :: Parser ()
lineEnd = blanks *> newline *> space -- at least one newline

list :: Parser [Term]
list = term `sepEndBy1` blanks

term :: Parser Term
term = literal <|> RuleRef <$> (lt *> ruleName <* gt)

literal :: Parser Term
literal = Literal <$> (dq *> text1 <* dq <|> sq *> text2 <* sq)

text1 :: Parser String
text1 = many character1

text2 :: Parser String
text2 = many character2

character :: Parser Char
character = letterChar <|> digitChar <|> symbol'

symbol' :: Parser Char
symbol' = oneOf availableSpecialChars

character1 :: Parser Char
character1 = character <|> sq

character2 :: Parser Char
character2 = character <|> dq

ruleName :: Parser String
ruleName = do
  shead <- letterChar
  stail <- many ruleChar
  return $ shead : stail

ruleChar :: Parser Char
ruleChar = letterChar <|> digitChar <|> char '-'

blanks :: Parser ()
blanks = many (char ' ') >> pure ()

lt :: Parser Char
lt = char '<'

gt :: Parser Char
gt = char '>'

sq :: Parser Char
sq = char '\''

dq :: Parser Char
dq = char '"'
