{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.BackusNaurForm where

import Control.Applicative
import Data.Aeson (
  ToJSON, genericToEncoding, defaultOptions, toEncoding, pairs, (.=))
import Data.List (intercalate)
import GHC.Generics
import Test.QuickCheck (Arbitrary, arbitrary, choose, frequency, Gen, sized)
import Text.Megaparsec
import Text.Megaparsec.String
import Parser.Util


data BNFDefinition = BNFDefinition {
    rules :: [RuleDefinition]
  } deriving (Generic, Eq)
  
instance Show BNFDefinition where
  show (BNFDefinition rs) = intercalate "\n" $ map show rs
  
instance Monoid BNFDefinition where
  mempty = BNFDefinition []
  mappend (BNFDefinition a) (BNFDefinition b) = 
    BNFDefinition $ a ++ b

instance ToJSON BNFDefinition where
  toEncoding = genericToEncoding defaultOptions
  
instance Arbitrary BNFDefinition where
  arbitrary = BNFDefinition <$> arbitraryList (1, 10)


data RuleDefinition = RuleDefinition {
    name :: String
  , expr :: [[Term]] -- semantically "or"-connected lists
  } deriving (Generic, Eq)
  
instance Show RuleDefinition where
  show (RuleDefinition n e) = "<" ++ n ++ "> ::= " ++ (intercalate " | " $ map (intercalate " ") $ (map . map) show e)

instance ToJSON RuleDefinition where
  toEncoding = genericToEncoding defaultOptions
  
instance Arbitrary RuleDefinition where
  arbitrary = RuleDefinition <$> ruleNameGen <*> genList (1, 6) (arbitraryList (1, 4))
  

data Term = Literal String | RuleRef String
  deriving (Generic, Eq)
  
instance Show Term where
  show (RuleRef s) = "<" ++ s ++ ">"
  show (Literal s) = 
    let q = if elem '\'' s then "\"" else "'"
    in  q ++ s ++ q

instance ToJSON Term where
  toEncoding (Literal s) = pairs $ "l" .= s
  toEncoding (RuleRef s) = pairs $ "ref" .= s
  
instance Arbitrary Term where
  arbitrary = frequency [
    (1, Literal <$> literalGen),
    (1, RuleRef <$> ruleNameGen)]

data ParseErr = ParseErr {
    error :: String
  } deriving (Generic, Eq, Show)

instance ToJSON ParseErr where
  toEncoding = genericToEncoding defaultOptions
  
parseString :: String -> Either (ParseError Char Dec) BNFDefinition
parseString = parse syntax ""

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
symbol' = oneOf specialChars

specialChars :: [Char]
specialChars = "| !#$%&()*+,-./:;>=<?@[\\]^_`{}~"

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


-- generation utility methods

freqFromChars :: [Char] -> Gen Char
freqFromChars c = frequency $ map((,) 1 . return) c

-- list generation from https://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
arbitraryList :: (Arbitrary a) => (Int, Int) -> Gen [a]
arbitraryList = flip genList arbitrary

genList :: (Int, Int) -> Gen a -> Gen [a]
genList (minL, maxL) g = sized $ \n -> do
  k <- choose (minL, min (max minL n) maxL)
  sequence [ g | _ <- [1..k] ]
  
ruleNameGen :: Gen String
ruleNameGen = let 
    avLetters = ['a'..'z'] ++ ['A'..'Z'] 
    avDigits  = ['0'..'9']
  in do
    begin <- freqFromChars avLetters
    remainder <- genList (0, 10) $ freqFromChars $ '-' : avLetters ++ avDigits
    return $ begin : remainder
    
literalGen :: Gen String
literalGen = 
  let avChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ specialChars
                -- literal may contain either ' or " but never both
      listGen = genList (0, 20) . freqFromChars . flip (:) avChars
  in  frequency [(1, listGen '\''), (1, listGen '"')]
