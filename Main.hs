{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified BackusNaurForm as BNF
import Data.ByteString.Lazy.Char8
import Data.Maybe
import Network.HTTP.Types.Status
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  post "/parse" $ do
    b <- body
    parsed <- pure $ BNF.parse $ unpack b
    status $ if parsed == Nothing then status400 else status200
    json $ fromMaybe mempty parsed