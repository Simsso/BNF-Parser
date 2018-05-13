{-# OPTIONS_GHC -Wall #-}

module Parser.BNF (
    parseString
  , module Parser.BNF.Constants
  , module Parser.BNF.Parsers
  , module Parser.BNF.Types
) where

import Parser.BNF.Constants
import Parser.BNF.Parsers (syntax)
import Parser.BNF.Types

import Text.Megaparsec


parseString :: String -> Either (ParseError Char Dec) BNFDefinition
parseString = parse syntax ""


