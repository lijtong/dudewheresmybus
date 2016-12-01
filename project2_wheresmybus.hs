-- {-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
--{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char
import qualified Data.Text as T
import Text.XML.HXT.Core
import Data.Aeson
import GHC.Generics
import Data.Typeable
import Data.Data

key' = "cURsrZ7AjbHgfC8cuZLV"
-- url = "http://api.translink.ca/rttiapi/v1/stops/52026/estimates?apikey=" ++ key'
url = "http://api.translink.ca/RTTIAPI/V1/stops/55612?apiKey=cURsrZ7AjbHgfC8cuZLV"
--Just uri = parseURI url
header' = Header HdrAccept "application/JSON"

--request = simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody

 
data BusEstimate = BusEstimate 
  { destination :: [Char], 
    expectedleavetime :: [Char],
    cancelledtrip :: [Char] }
  deriving (Show, Typeable, Generic)

data BusStop = BusStop
  { stopno, 
    name,
    bayno,
    city,
    onstreet,
    atstreet,
    latitude,
    longitude,
    wheelchairaccess,
    distance,
    routes :: [Char] }
  deriving (Show, Typeable, Generic)
  
instance FromJSON BusStop
instance ToJSON BusStop 

-- preface: program runs in terms of ACTIONS and do blocks.
--            as one action is finished, you link it to another action.

-- note: the last line of "do" blocks are required to be "Actions"; i.e. we can't just output a number or string. CTRL-F and see the last line with "!!!". Our code won't compile without this.

-- note: This feels scraggily, or weak. I'm wondering if there's another way to go about this. This is a start, however.

-- first action called. Will give introductory information before action 1 is called.
main = do
  putStrLn "Hello! Welcome to where's my bus! We'll get you your bus schedule."
  main1

-- prompt user for valid input. Move to step 2 if bus is valid! Otherwise, loop.
main1 = do
  bus <- getLine

  if (isValidBus bus) then
    main2
  else
    do
      putStrLn "Invalid Bus #, try again!"
      main1

-- action 2: API work goes here
main2 = do
  -- something
  putStrLn "Finished" -- !!!

-- main3 = do
  -- doc <- getBusData
  -- xml <- return $ parseXML doc
  -- result <- runX (xml >>> getStop)
  -- putStrLn doc
  -- case result of
    -- []  -> putStrLn "Unable to parse bus data."
    -- w:_ -> print w

main3 = do
 getStop
------------------------------
-- Helper functions belong below, so as to make our "Actions" more clear and simple.

-- take string. trim string. Check only if it's 5 digits and all numbers.
isValidBus :: Foldable t => t Char -> Bool
isValidBus xs = (all isDigit xs) && (isValidLength xs)

-- check if string's length is 5.
isValidLength :: Foldable t => t a -> Bool
isValidLength xs = length xs == 5

-- parseXML doc = readString [ withValidate no
                          -- , withRemoveWS yes
                          -- ] doc

-- atTag tag = deep (isElem >>> hasName tag)
-- text = getChildren >>> getText
-- textAtTag tag = atTag tag >>> text

-- getEstimate = atTag "NextBuses" >>>
  -- proc x -> do
    -- dest     <- textAtTag "destination"          -< x
    -- expLeave <- textAtTag "expectedleavetime"  -< x
    -- cancel    <- textAtTag "cancelledtrip"           -< x
    -- returnA -< BusEstimate 
      -- { destination        = dest,
        -- expectedleavetime = expLeave,
        -- cancelledtrip      = cancel }

-- getStop = atTag "Stop" >>>
  -- proc x -> do
    -- stopnum     <- textAtTag "stopno"          -< x
    -- name_ <- textAtTag "name"  -< x
    -- baynum    <- textAtTag "bayno"           -< x
    -- city_     <- textAtTag "city"          -< x
    -- on     <- textAtTag "onstreet"          -< x
    -- at     <- textAtTag "atstreet"          -< x
    -- lat     <- textAtTag "latitude"          -< x
    -- long     <- textAtTag "longitude"          -< x
    -- wheel     <- textAtTag "wheelchairaccess"          -< x
    -- dist     <- textAtTag "distance"          -< x
    -- routes_     <- textAtTag "routes"          -< x
    -- returnA -< BusStop 
      -- { stopno        = stopnum,
        -- name = name_,
        -- bayno      = baynum, 
        -- city      = city_, 
        -- onstreet      = on, 
        -- atstreet      = at, 
        -- latitude      = lat, 
        -- longitude      = long, 
        -- wheelchairaccess      = wheel, 
        -- distance      = dist, 
        -- routes      = routes_ }
getStop :: IO (Maybe BusStop)		
getStop = fmap decode $ getBusData

getBusData :: IO (C.ByteString)		
getBusData = do
  case parseURI url of
    Nothing  -> ioError . userError $ "Invalid URL"
    Just uri -> get uri
 
get :: URI -> IO (C.ByteString)
get uri = do
  eresp <- simpleHTTP (Request uri GET [header'] "")
  case eresp of
    Left _    -> ioError . userError $ "Failed to get " ++ show uri
    Right res -> return $ C.pack(rspBody res)

-- http://api.translink.ca/RTTIAPI/V1/stops/55612?apiKey=cURsrZ7AjbHgfC8cuZLV
