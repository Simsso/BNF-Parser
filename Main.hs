{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified BackusNaurForm as BNF
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8
import Network.HTTP.Types.Status
import Web.Scotty


main :: IO ()
main = scotty 3000 $ do
  post "/parse" $ do
    b <- body
    parsed <- pure $ BNF.parse $ unpack b
    status status200
    respond parsed

respond :: (Aeson.ToJSON a, Aeson.ToJSON b) => Either a b -> ActionM ()
respond (Right definition) = do
  status status200
  json $ definition
respond (Left  failure) = do
  status status400
  json $ failure