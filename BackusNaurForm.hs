{-# OPTIONS_GHC -Wall #-}

module BackusNaurForm where

import Control.Applicative
import Text.Trifecta
import Util


data BNFDefinition = BNFDefinition [RuleDefinition]
  deriving (Eq, Show)

data RuleDefinition = RuleDefinition RuleName Expression
  deriving (Eq, Show)

newtype Expression = Expression [[Term]] -- or connected term lists
  deriving (Eq, Show)
  
newtype RuleName = RuleName String
  deriving (Eq, Show)

data Term = Literal String | Rule RuleName
  deriving (Eq, Show)


syntax :: Parser BNFDefinition
syntax = BNFDefinition <$> (rule `sepEndBy1` lineEnd) <* eof 

rule :: Parser RuleDefinition
rule = do
  _    <- blanks >> char '<'
  name <- ruleName <* char '>' <* blanks <* string "::=" <* blanks
  expr <- expression
  return $ RuleDefinition (RuleName name) expr

expression :: Parser Expression
expression = try orConcatenatedList <|> singleList
               
singleList :: Parser Expression
singleList = Expression . (: []) <$> list

orConcatenatedList :: Parser Expression
orConcatenatedList = do
  l <- list <* (blanks >> char '|' >> blanks)
  Expression e <- expression
  return $ Expression $ l : e
  
blanks :: Parser ()
blanks = const () <$> (many $ char ' ')

lineEnd :: Parser ()
lineEnd = do
  _ <- blanks >> some newline
  return ()

list :: Parser [Term]
list = try (do
  h <- term <* blanks
  t <- list
  return $ h : t) <|> (: []) <$> term

term :: Parser Term
term = literal <|> 
  (Rule . RuleName <$> (char '<' >> ruleName <* char '>'))

literal :: Parser Term
literal = Literal <$> ((cp2s (char  '"') >> text1 <* cp2s (char  '"')) <|> 
                       (cp2s (char '\'') >> text2 <* cp2s (char '\'')))

text1 :: Parser String
text1 = many character1

text2 :: Parser String
text2 = many character2

character :: Parser Char
character = letter' <|> digit' <|> symbol'

letter' :: Parser Char
letter' = oneOf $ ['A'..'Z'] ++ ['a'..'z']

digit' :: Parser Char
digit' = oneOf ['0'..'9']

symbol' :: Parser Char
symbol' = oneOf "| !#$%&()*+,-./:;>=<?@[\\]^_`{}~"

character1 :: Parser Char
character1 = character <|> char '\''

character2 :: Parser Char
character2 = character <|> char '"'

ruleName :: Parser String
ruleName = do
  shead <- letter'
  stail <- many ruleChar
  return $ shead : stail

ruleChar :: Parser Char
ruleChar = letter' <|> digit <|> char '-'
