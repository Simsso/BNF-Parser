{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified BackusNaurForm as BNF
import Data.ByteString.Lazy.Char8
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  post "/parse" $ do
    b <- body
    json $ BNF.parse $ unpack b