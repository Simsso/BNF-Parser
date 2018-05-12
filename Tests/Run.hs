{-# OPTIONS_GHC -Wall #-}

module Tests.Run where

import Test.Hspec
import qualified Tests.IntegrationTest
import qualified Tests.ParserTest
import qualified Tests.PropertyTest

main :: IO ()
main = hspec $ do
  describe "Parser tests" $ Tests.ParserTest.run
  describe "Integration tests" $ Tests.IntegrationTest.run
  describe "Property tests" $ Tests.PropertyTest.run
