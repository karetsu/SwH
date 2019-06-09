{-# LANGUAGE OverloadedStrings #-}
-- | Simple GET and parse of test and JSON
module Network.GetJson where

-- web interface and json
import Network.Wreq
import Control.Lens

import Data.Aeson
import Data.Aeson.Lens

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Internal as B

-- | Fetching content of a URL into ByteString
geturl :: String -> IO (Maybe B.ByteString)
geturl url = do
  r <- getWith defaults url
  return $ r ^? responseBody


-- | fetching shakespeare plain text
shakeUrl :: String
shakeUrl = "https://ocw.mit.edu/ans7870/6/6.006/s08/lecturenotes/files/t8.shakespeare.txt"

shakespeare :: IO (Maybe B.ByteString)
shakespeare = geturl shakeUrl


-- | fetching some JSON
jasonUrl :: String
jasonUrl = "https://raw.githubusercontent.com/h-Klok/StatsWithJuliaBook/master/1_chapter/jsonCode.json"

jason :: IO (Maybe B.ByteString)
jason = geturl jasonUrl
