{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as C
import Text.XML.Light

key' = "cURsrZ7AjbHgfC8cuZLV"
url = "http://api.translink.ca/rttiapi/v1/stops/52026/estimates?apikey=" ++ key'
Just uri = parseURI url
header' = Header HdrAccept "application/JSON"

request = simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody