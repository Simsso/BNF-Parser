{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BackusNaurForm where

import Control.Applicative
import Data.Aeson (
  ToJSON, genericToEncoding, defaultOptions, toEncoding, pairs, (.=))
import GHC.Generics
import Text.Trifecta
import Util



data BNFDefinition = BNFDefinition {
    rules :: [RuleDefinition]
  } deriving (Generic, Eq, Show)
  
instance Monoid BNFDefinition where
  mempty = BNFDefinition []
  mappend (BNFDefinition a) (BNFDefinition b) = 
    BNFDefinition $ a ++ b

instance ToJSON BNFDefinition where
  toEncoding = genericToEncoding defaultOptions


data RuleDefinition = RuleDefinition {
    name :: String
  , expr :: [[Term]] -- semantically "or"-connected lists
  } deriving (Generic, Eq, Show)

instance ToJSON RuleDefinition where
  toEncoding = genericToEncoding defaultOptions
  

data Term = Literal String | RuleRef String
  deriving (Generic, Eq, Show)

instance ToJSON Term where
  toEncoding (Literal s) = pairs $ "l" .= s
  toEncoding (RuleRef s) = pairs $ "ref" .= s
  

parse :: String -> Maybe BNFDefinition
parse s = go $ parseString syntax mempty s where
  go (Success definition) = Just definition
  go (Failure _) = Nothing
  

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
lineEnd = blanks *> some newline *> blanks

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
