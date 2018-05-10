{-# OPTIONS_GHC -Wall #-}

module Tests.IntegrationTest where
  
import Data.Either (isRight)
import qualified Parser.BackusNaurForm as BNF
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
  let folder = if desired then "Valid" else "Invalid"
      notW   = if desired then " " else " not "
  in do
    it ("should" ++ notW ++ "parse \"" ++ file ++ "\"") $ do
      s <- readFile $ "Samples/" ++ folder ++ "/" ++ file ++ ".txt"
      isRight (BNF.parse s) `shouldBe` desired
