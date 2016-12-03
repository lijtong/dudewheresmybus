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
import Data.Char
import Control.Monad
import System.Exit
import Data.String
import qualified Data.Text as T

testurl = "http://maps.googleapis.com/maps/api/geocode/json?address=2366+Main+Mall,+Vancouver,+BC"
key' = "cURsrZ7AjbHgfC8cuZLV"
url = "http://api.translink.ca/rttiapi/v1/stops"
geo = "http://maps.googleapis.com/maps/api/geocode/json?address="
estimates = "estimates?apikey=" ++ key'
routeno = "&routeno="
header' = Header HdrAccept "application/JSON"
n = -1

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
	putStrLn "Enter the radius that you would like to search in metres"
	rad <- getLine
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
	let queryurl = url ++ "?" ++ "apikey=" ++ key' ++ "&lat=" ++ (show proplat) ++ "&long=" ++ (show proplong) ++ "&radius=" ++ rad
	let Just uri = parseURI queryurl	
	--print uri
	estquery <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
	printstops estquery
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

printstops estquery = do 
	let checkquery = ('[':estquery)
	let checkquery1 = checkquery ++ "]"
	let estcheck = read checkquery1 :: JSON
	let code = [jsq| estcheck[0].Code |] :: String
	let msg = [jsq| estcheck[0].Message |] :: String
	let estjson = read estquery :: JSON
	if (code == "null") then do
		let name1 = [jsq| estjson[0].Name |] :: String
		let routes1 = [jsq| estjson[0].Routes |]     :: String
		let stop1 = [jsq| estjson[0].StopNo |]     :: Integer
		let name2 = [jsq| estjson[1].Name |] :: String
		let routes2 = [jsq| estjson[1].Routes |]     :: String
		let stop2 = [jsq| estjson[1].StopNo |]     :: Integer
		let name3 = [jsq| estjson[2].Name |] :: String
		let routes3 = [jsq| estjson[2].Routes |]     :: String
		let stop3 = [jsq| estjson[2].StopNo |]     :: Integer
		let name4 = [jsq| estjson[3].Name |] :: String
		let routes4 = [jsq| estjson[3].Routes |]     :: String
		let stop4 = [jsq| estjson[3].StopNo |]     :: Integer
		let name5 = [jsq| estjson[4].Name |] :: String
		let routes5 = [jsq| estjson[4].Routes |]     :: String
		let stop5 = [jsq| estjson[4].StopNo |]     :: Integer
		let name6 = [jsq| estjson[5].Name |] :: String
		let routes6 = [jsq| estjson[5].Routes |]     :: String
		let stop6 = [jsq| estjson[5].StopNo |]     :: Integer
		let name7 = [jsq| estjson[6].Name |] :: String
		let routes7 = [jsq| estjson[6].Routes |]     :: String
		let stop7 = [jsq| estjson[6].StopNo |]     :: Integer
		let name8 = [jsq| estjson[7].Name |] :: String
		let routes8 = [jsq| estjson[7].Routes |]     :: String
		let stop8 = [jsq| estjson[7].StopNo |]     :: Integer
		let name9 = [jsq| estjson[8].Name |] :: String
		let routes9 = [jsq| estjson[8].Routes |]     :: String
		let stop9 = [jsq| estjson[8].StopNo |]     :: Integer
		let name10 = [jsq| estjson[9].Name |] :: String
		let routes10 = [jsq| estjson[9].Routes |]     :: String
		let stop10 = [jsq| estjson[9].StopNo |]     :: Integer
		if not (name1 == "null") then do
			putStrLn name1
			putStrLn routes1
			print stop1
			if not (name2 == "null") then do
				putStrLn name2
				putStrLn routes2
				print stop2
				if not (name3 == "null") then do
					putStrLn name3
					putStrLn routes3
					print stop3
					if not (name4 == "null") then do
						putStrLn name4
						putStrLn routes4
						print stop4
						if not (name5 == "null") then do
							putStrLn name5
							putStrLn routes5
							print stop5
							if not (name6 == "null") then do
								putStrLn name6
								putStrLn routes6
								print stop6
								if not (name7 == "null") then do
									putStrLn name7
									putStrLn routes7
									print stop7
									if not (name8 == "null") then do
										putStrLn name8
										putStrLn routes8
										print stop8
										if not (name9 == "null") then do
											putStrLn name9
											putStrLn routes9
											print stop9
											if not (name10 == "null") then do
												putStrLn name10
												putStrLn routes10
												print stop10
												else putStrLn "Done"
											else putStrLn "Done"
										else putStrLn "Done"
									else putStrLn "Done"
								else putStrLn "Done"
							else putStrLn "Done"
						else putStrLn "Done"
					else putStrLn "Done"
				else putStrLn "Done"
			else putStrLn "Done"
		else putStrLn msg
