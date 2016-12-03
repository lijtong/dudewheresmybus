-- !!! COPIED IMPORT FOR DeadSimpleJSON
{-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}

module Main where


import Text.Printf (printf)

import Text.DeadSimpleJSON
import Text.DeadSimpleJSON.TH

import Data.Ratio
-- COPIED END

import Network.HTTP
import Network.HTTP.Headers
import Network.URI
--import qualified Data.ByteString.Lazy.Char8 as C
--import Text.XML.Light
import Data.Char
--import Control.Concurrent
import Control.Monad
import System.Exit
import Data.String
import qualified Data.Text as T
--import Data.Typeable

testurl = "http://maps.googleapis.com/maps/api/geocode/json?address=2366+Main+Mall,+Vancouver,+BC"
key' = "cURsrZ7AjbHgfC8cuZLV"
-- geocachekey = "AIzaSyAhSWqsa_BRpEJtM5FGhSa3ZGJjQ0FWByg"
url = "http://api.translink.ca/rttiapi/v1/stops"
geo = "http://maps.googleapis.com/maps/api/geocode/json?address="
estimates = "estimates?apikey=" ++ key'
routeno = "&routeno="
header' = Header HdrAccept "application/JSON"

-- preface: program runs in terms of ACTIONS and do blocks.
--            as one action is finished, you link it to another action.

-- !!!
main = do                                                                               -- sequence IO operations
  putStrLn "Hello! Welcome to Where's My Bus! We'll get you your bus schedule."         -- brief intro
  choice                                                                                -- do "choice" block 
  
choice = forever $ do 
  putStrLn "Enter 1 for a regular bus stop estimate"                                    -- shows up on prompt for user to know 1 => bus stop
  putStrLn "Enter 2 for a geolocation bus stop search"                                  -- shows up on prompt for user to know 2 => latlon
  putStrLn "Enter exit to exit this program"                                            -- shows up on prompt for user to know how to exit
  choicenum <- getLine                                                                  -- initialize choicenum as the command user inserts in
  if choicenum == "1" then main1                                                        -- if 1 is entered, do main1 block
  else if choicenum == "2" then main2                                                   -- if 2 is entered, do main2 block
  else if choicenum == "exit" then exitWith ExitSuccess                                 -- if exit is entered, exit the main function 
  else putStrLn "Sorry I didn't recognize your choice"                                  -- invalidates all other non applicable actions
  
main1 = do
  putStrLn "Input your bus stop number now"                                             -- allows user to know it is looking for a 5-numbered bus stop
  bstop <- getLine                                                                      -- bstop as the command user inserts in
  if (not (isValidBus bstop)) then do                                                   -- checks if BusStop number is valid (5 digit numbers)
		putStrLn "Invalid bus #, try again!"                                    -- invalid case: prints out that it is invalid
                choice                                                                  -- 
  else  putStrLn "Input your route number now"
  broute <- getLine
  if (not (isValidRoute broute)) then do 
		putStrLn "Invalid route #, try again!"
		choice
  else do
	let queryurl = url ++ "/" ++ bstop ++ "/" ++ estimates ++ routeno ++ broute
	let Just uri = parseURI queryurl	
	-- print uri
	estquery <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
	let checkquery = ('[':estquery)
	let checkquery1 = checkquery ++ "]"
	let estcheck = read checkquery1 :: JSON
	let code = [jsq| estcheck[0].Code |] :: String
	let msg = [jsq| estcheck[0].Message |] :: String
	-- print val
	let estjson = read estquery :: JSON
	-- print estjson
	-- let val = [jsq| estjson[0].Code |] :: String
	-- print val
	if (code == "null") then do
	-- print estjson
		let val1 = [jsq| estjson[0].Direction |] :: String
		let val2 = [jsq| estjson[0].Schedules[0].Destination |]     :: String
		let val3 = [jsq| estjson[0].Schedules[0].ExpectedLeaveTime |]     :: String
		let val4 = [jsq| estjson[0].Schedules[1].Destination |]     :: String
		let val5 = [jsq| estjson[0].Schedules[1].ExpectedLeaveTime |]     :: String
		let val6 = [jsq| estjson[0].Schedules[2].Destination |]     :: String
		let val7 = [jsq| estjson[0].Schedules[2].ExpectedLeaveTime |]     :: String
		putStrLn val1
		putStrLn val2
		putStrLn val3
		putStrLn val4
		putStrLn val5
		putStrLn val6
		putStrLn val7
	else putStrLn msg

main2 = do
	putStrLn "Enter the address you would like to know nearby bus stops for"
	addr <- getLine
	let formataddr = replaceBlank addr
	let geourl = geo ++ formataddr
	let Just geouri = parseURI geourl
	-- print geouri
	geoquery <- simpleHTTP (Request geouri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
	-- print geoquery
	let mod = ('[':geoquery)
	let mod1 = mod ++ "]"
	-- print change1
	let mod2 = T.replace (T.pack("\n")) (T.pack("")) (T.pack(mod1))
	-- print what
	let mod3 = T.replace (T.pack(" ")) (T.pack("")) mod2
	-- print what1
	let geojson = read (T.unpack(mod3)) :: JSON
	-- print geojson
	let lat = [jsq| geojson[0].results[0].geometry.location.lat |] :: Double
	let long = [jsq| geojson[0].results[0].geometry.location.lng |] :: Double
	let proplat = (truncate' lat 6)
	let proplong = (truncate' long 6)
	let queryurl = url ++ "?" ++ "apikey=" ++ key' ++ "&lat=" ++ (show proplat) ++ "&long=" ++ (show proplong) ++ "&radius=500"
	let Just uri = parseURI queryurl	
	--print uri
	estquery <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
	let checkquery = ('[':estquery)
	let checkquery1 = checkquery ++ "]"
	let estcheck = read checkquery1 :: JSON
	let code = [jsq| estcheck[0].Code |] :: String
	let msg = [jsq| estcheck[0].Message |] :: String
	let estjson = read estquery :: JSON
	print estjson
------------------------------
-- Helper functions belong below, so as to make our "Actions" more clear and simple.

-- take string. trim string. Check only if it's 5 digits and all numbers.
isValidBus :: Foldable t => t Char -> Bool
isValidBus xs = (all isDigit xs) && (isValidLength xs)

isValidRoute :: Foldable t => t Char -> Bool
isValidRoute xs = (all isDigit xs) && (isValidLengthRoute xs)

-- check if string's length is 5.
isValidLength :: Foldable t => t a -> Bool
isValidLength xs = length xs == 5

isValidLengthRoute :: Foldable t => t a -> Bool
isValidLengthRoute xs = length xs == 3

-- Function to properly format the location requests
replaceBlank = map (\c -> if c==' ' then '+'; else c)

-- Truncate function that rounds doubles to specified number of decimal places, from StackOverflow answer
-- http://stackoverflow.com/a/31952975
truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n
