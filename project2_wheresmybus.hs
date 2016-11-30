import Network.HTTP

key = "cURsrZ7AjbHgfC8cuZLV"

getStop = simpleHTTP (getRequest ("http://api.translink.ca/rttiapi/v1/stops/52026/estimates?apikey=" ++ key)) >>= fmap (take 10000) . getResponseBody