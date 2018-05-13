{-# OPTIONS_GHC -Wall #-}

import Test.Hspec
import qualified IntegrationTest
import qualified ParserTest
import qualified PropertyTest

main :: IO ()
main = hspec $ do
  describe "Parser tests" $ ParserTest.run
  describe "Integration tests" IntegrationTest.run
  describe "Property tests" PropertyTest.run