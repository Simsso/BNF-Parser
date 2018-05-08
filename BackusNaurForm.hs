{-# OPTIONS_GHC -Wall #-}

module BackusNaurForm where

import Control.Applicative
import Text.Trifecta
import Util


data BNFDefinition = BNFDefinition [RuleDefinition]
  deriving (Eq, Show)

data RuleDefinition = RuleDefinition String Expression
  deriving (Eq, Show)

newtype Expression = Expression [[Term]] -- or connected term lists
  deriving (Eq, Show)

data Term = Literal String | RuleRef String
  deriving (Eq, Show)
  

syntax :: Parser BNFDefinition
syntax = BNFDefinition <$> rule `sepEndBy1` lineEnd <* eof 

rule :: Parser RuleDefinition
rule = do
  name <- blanks *> lt *> ruleName <* gt <* blanks
  expr <- string "::=" *> blanks *> expression
  return $ RuleDefinition name expr

expression :: Parser Expression
expression = Expression <$> list `sepBy1` (blanks *> char '|' *> blanks)

lineEnd :: Parser ()
lineEnd = blanks *> some newline *> mempty

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
character = letter <|> digit <|> symbol'

symbol' :: Parser Char
symbol' = oneOf "| !#$%&()*+,-./:;>=<?@[\\]^_`{}~"

character1 :: Parser Char
character1 = character <|> sq

character2 :: Parser Char
character2 = character <|> dq

ruleName :: Parser String
ruleName = do
  shead <- letter
  stail <- many ruleChar
  return $ shead : stail

ruleChar :: Parser Char
ruleChar = letter <|> digit <|> char '-'

blanks :: Parser ()
blanks = many (char ' ') *> mempty
