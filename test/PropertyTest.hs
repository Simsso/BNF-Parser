{-# OPTIONS_GHC -Wall #-}

module PropertyTest where
  
import Parser.BNF
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)
import Test.QuickCheck


propertyInverseFunction :: BNFDefinition -> Bool
propertyInverseFunction bnf = case parseString (show bnf) of
  (Left _) -> False
  (Right bnf') -> bnf' == bnf


run :: SpecM () ()
run = do
  describe "parse" $ do
    it "is inverse to show" $ property $
      propertyInverseFunction
