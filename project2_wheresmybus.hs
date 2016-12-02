-- !!! COPIED IMPORT
{-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}

module Main where


import Text.Printf (printf)

import Text.DeadSimpleJSON
import Text.DeadSimpleJSON.TH

import Data.Ratio
-- COPIED END

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as C
import Text.XML.Light
import Data.Char
import Control.Concurrent

testurl = "http://maps.googleapis.com/maps/api/geocode/json?address=2366+Main+Mall,+Vancouver,+BC"
key' = "cURsrZ7AjbHgfC8cuZLV"
geocachekey = "AIzaSyAhSWqsa_BRpEJtM5FGhSa3ZGJjQ0FWByg"
url = "http://api.translink.ca/rttiapi/v1/stops/"
--url = "http://api.translink.ca/rttiapi/v1/stops/51947/estimates?apikey=" ++ key'
estimates = "estimates?apikey=" ++ key'
--Just uri = parseURI url
header' = Header HdrAccept "application/JSON"

--request = simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody

-- !!! COPIED

-- COPIED END


-- preface: program runs in terms of ACTIONS and do blocks.
--            as one action is finished, you link it to another action.

-- note: the last line of "do" blocks are required to be "Actions"; i.e. we can't just output a number or string. CTRL-F and see the last line with "!!!". Our code won't compile without this.

-- note: This feels scraggily, or weak. I'm wondering if there's another way to go about this. This is a start, however.

-- first action called. Will give introductory information before action 1 is called.
-- !!!
main = do
  putStrLn "Hello! Welcome to where's my bus! We'll get you your bus schedule."
  main1

main1 = do
  putStrLn "Input your bus stop number now"
  bstop <- getLine
  if (not (isValidBus bstop)) then do 
		putStrLn "Invalid Bus #, try again!"
		main1
  else do
	let queryurl = url ++ bstop ++ "/" ++ estimates
	let Just uri = parseURI queryurl	
	print uri
	test <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
  -- !!!! CURRENTLY, test acts as a string.
  -- putStrLn test
	-- let jsonData = read test :: JSON
	-- print $ jsonData
	-- writeFile "data'.json" test
	-- threadDelay 5000000
  -- let jone = [json| $ show jsonData |]
  --     val1 = [jsq| jone[1].Direction |] :: String
  -- putStrLn $ show val1
      -- -- putStrLn $ show jsonData
      -- print $ (jsonData ? "[0]" :: JSON)
--	let jone = [jsonF|data'.json|]
	-- Able to read without writing to file now
	let jone = read test :: JSON
	let val1 = [jsq| jone[0].Direction |] :: String
	let val2 = [jsq| jone[0].Schedules[0].Destination |]     :: String
	let val3 = [jsq| jone[0].Schedules[0].ExpectedLeaveTime |]     :: String
	let val4 = [jsq| jone[0].Schedules[1].Destination |]     :: String
	let val5 = [jsq| jone[0].Schedules[1].ExpectedLeaveTime |]     :: String
	let val6 = [jsq| jone[0].Schedules[2].Destination |]     :: String
	let val7 = [jsq| jone[0].Schedules[2].ExpectedLeaveTime |]     :: String
	putStrLn val1
	putStrLn val2
	putStrLn val3
	putStrLn val4
	putStrLn val5
	putStrLn val6
	putStrLn val7

-- prompt user for valid input. Move to step 2 if bus is valid! Otherwise, loop.
-- main1 = do
 -- bus <- getLine  
  -- if (isValidBus bus) then

  -- main2
  -- else
    -- do
      -- putStrLn "Invalid Bus #, try again!"
      -- main1

-- -- action 2: API work goes here
-- main2 = do
  -- -- something
  -- putStrLn "Finished" -- !!!

------------------------------
-- Helper functions belong below, so as to make our "Actions" more clear and simple.

-- take string. trim string. Check only if it's 5 digits and all numbers.
isValidBus :: Foldable t => t Char -> Bool
isValidBus xs = (all isDigit xs) && (isValidLength xs)

-- check if string's length is 5.
isValidLength :: Foldable t => t a -> Bool
isValidLength xs = length xs == 5




-- http://api.translink.ca/RTTIAPI/V1/stops/55612?apiKey=cURsrZ7AjbHgfC8cuZLV
