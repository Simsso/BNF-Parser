{-# OPTIONS_GHC -Wall #-}

module IntegrationTest where
  
import Data.Either (isRight)
import qualified Parser.BNF as BNF
import Test.Hspec
import Test.Hspec.Core.Spec (SpecM)


validFiles :: [String]
validFiles = ["BNFDefinition", "NewLinesWithSpaces", "PostalAddress"]

invalidFiles :: [String]
invalidFiles = ["ClosingGreaterThanMissing", "RuleMarker"]

run :: SpecM () ()
run = do
  
  describe "valid files" $ do
    let runAgainst' = runAgainst True
    sequence_ $ map runAgainst' validFiles
  
  describe "invalid files" $ do
    let runAgainst' = runAgainst False
    sequence_ $ map runAgainst' invalidFiles
    

runAgainst :: Bool -> String -> SpecM () ()
runAgainst desired file = 
  let folder = if desired then "valid" else "invalid"
      notW   = if desired then " " else " not "
  in do
    it ("should" ++ notW ++ "parse \"" ++ file ++ "\"") $ do
      s <- readFile $ "test/samples/" ++ folder ++ "/" ++ file ++ ".txt"
      isRight (BNF.parseString s) `shouldBe` desired