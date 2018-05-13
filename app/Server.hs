{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import qualified Parser.BNF as BNF
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy.Char8
import Network.HTTP.Types.Status
import Web.Scotty


start :: IO ()
start = scotty 3000 $ do
  get "/" $ file "app/webpage/index.html"
  post "/parse" parseController
  get  "/meta-data/api-spec" $ file "app/webpage/api-spec.yaml"
  get  "/meta-data/bnf-definition" $ file "app/webpage/bnf-definition.txt"

parseController :: ActionM ()
parseController = do
  b <- body
  parsed <- pure $ BNF.parseString $ unpack b
  status status200
  respond parsed where
    respond :: (Show a, Aeson.ToJSON b) => Either a b -> ActionM ()
    respond (Right definition) = do
      status status200
      json $ definition
    respond (Left failure) = do
      status status400
      json . BNF.ParseErr $ show failure