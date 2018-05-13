{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.BNF.Types where

import Data.Aeson (
  ToJSON, genericToEncoding, defaultOptions, toEncoding, pairs, (.=))
import Data.List (intercalate)
import GHC.Generics
import Parser.BNF.Util
import Test.QuickCheck (Arbitrary, arbitrary, frequency)

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
