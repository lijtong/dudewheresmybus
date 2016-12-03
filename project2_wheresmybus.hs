-- !!! COPIED IMPORT FOR DeadSimpleJSON
{-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}

module DudeWheresMyBus where


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

-- preface: program runs in terms of ACTIONS and do blocks.
--            as one action is finished, you link it to another action.

-- !!!
start = do                                                                               									-- sequence IO operations
  putStrLn "Hello! Welcome to Where's My Bus! We'll get you your bus schedule."         	-- brief intro
  choice                                                                                										-- do "choice" block 
  
choice = forever $ do 
  putStrLn "Enter 1 for a regular bus stop estimate"                                 					   	-- shows up on prompt for user to know 1 => bus stop
  putStrLn "Enter 2 for a geolocation bus stop search"                       			        		   	-- shows up on prompt for user to know 2 => latlon
  putStrLn "Enter exit to exit this program"                                        					    	-- shows up on prompt for user to know how to exit
  choicenum <- getLine                                                                  								-- initialize choicenum as the command user inserts in
  if choicenum == "1" then main1                                                        						-- if 1 is entered, do main1 block
  else if choicenum == "2" then main2                                                   					-- if 2 is entered, do main2 block
  else if choicenum == "exit" then exitWith ExitSuccess                                 				-- if exit is entered, exit the main function 
  else putStrLn "Sorry I didn't recognize your choice"                                  					-- invalidates all other non applicable actions
  
main1 = do
  putStrLn "Input your bus stop number now"                                         					    -- allows user to know it is looking for a 5-numbered bus stop
  bstop <- getLine                                                                    									-- bstop as the command user inserts in
  if (not (isValidBus bstop)) then do                                              						     	-- checks if BusStop number is valid (5 digit numbers)
		putStrLn "Invalid bus #, try again!"                                    									-- invalid case: prints out that it is invalid
                choice                                                                 					 					-- loops back to choice if invalid`
  else  putStrLn "Input your route number now"															-- asking user for route number input, which is has a max length of 3
  broute <- getLine																										-- read user input
  if (not (isValidRoute broute)) then do 																		-- check validity of the route given
		putStrLn "Invalid route #, try again!"																	-- loop back to choice if it is invalid
		choice
  else do
	let queryurl = url ++ "/" ++ bstop ++ "/" ++ estimates ++ routeno ++ broute			-- URL query formation from the given information to do the GET request from Translink API
	let Just uri = parseURI queryurl																				-- parse the URI from the URL
	estquery <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody -- Uses simpleHTTP to make a GET request to the Translink API
	let checkquery = ('[':estquery)																				-- next few lines used for formatting error messages sent from Translink, their formats are
	let checkquery1 = checkquery ++ "]"																		-- inconsistent so some changes had to be made for the parser to accept them
	let estcheck = read checkquery1 :: JSON
	let code = [jsq| estcheck[0].Code |] :: String															-- grab code and message if there is an error, if there wasn't an error code and msg return null
	let msg = [jsq| estcheck[0].Message |] :: String
	let estjson = read estquery :: JSON
	if (code == "null") then do																					-- error check, if there is no code print out the three bus stop estimates received from Translink
		let val1 = [jsq| estjson[0].Direction |] :: String													-- extract JSON data parsed and print it out
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
	else putStrLn msg																									-- if there is an error, print out the error message

main2 = do
	putStrLn "Enter the address you would like to know nearby bus stops for"					-- take user input for address and radius they want to search for bus stops in
	addr <- getLine
	putStrLn "Enter the radius that you would like to search in metres"
	rad <- getLine
	let formataddr = replaceBlank addr																		-- call formatBlank to format the user input correctly to use with the query
	let geourl = geo ++ formataddr
	let Just geouri = parseURI geourl
	geoquery <- simpleHTTP (Request geouri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody -- send a query with user's address input to Google map API to resolve lat and long
	let mod = ('[':geoquery)																						-- formatting issues as Google sends the data back in a format unrecognized by the parser we chose, next few lines are for resolving
	let mod1 = mod ++ "]"
	let mod2 = T.replace (T.pack("\n")) (T.pack("")) (T.pack(mod1))
	let mod3 = T.replace (T.pack(" ")) (T.pack("")) mod2
	let geojson = read (T.unpack(mod3)) :: JSON
	let lat = [jsq| geojson[0].results[0].geometry.location.lat |] :: Double						-- extract lat and long from the JSON data returned
	let long = [jsq| geojson[0].results[0].geometry.location.lng |] :: Double
	let proplat = (truncate' lat 6)																					-- helper function truncate' used to reduce number of decimal places, can only have a max of 6 for Translink API
	let proplong = (truncate' long 6)
	let queryurl = url ++ "?" ++ "apikey=" ++ key' ++ "&lat=" ++ (show proplat) ++ "&long=" ++ (show proplong) ++ "&radius=" ++ rad -- URL formation to query Translink APi for geolocation stop lookup
	let Just uri = parseURI queryurl	
	estquery <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody -- using simpleHTTP, we query Translink API to get the appropriate stops that are within the radius specified`
	printstops estquery																								-- helper function printstops used to print out at most 10 stops received from the query, very crude implementation as we could not
																																-- iterate through the list due to parser limitations
------------------------------
-- Helper functions belong below, so as to make our "Actions" more clear and simple.

-- take string. trim string. Check only if it's 5 digits and all numbers.
isValidBus :: Foldable t => t Char -> Bool
isValidBus xs = (all isDigit xs) && (isValidLength xs)

-- check if route given is valid, we did not account for bus routes that had letters in the front (i.e night or community buses)
isValidRoute :: Foldable t => t Char -> Bool
isValidRoute xs = (all isDigit xs) && (isValidLengthRoute xs)

-- check if string's length is 5.
isValidLength :: Foldable t => t a -> Bool
isValidLength xs = length xs == 5

-- check if string is of length 3 for bus route
isValidLengthRoute :: Foldable t => t a -> Bool
isValidLengthRoute xs = length xs == 3

-- Function to properly format the location requests
replaceBlank = map (\c -> if c==' ' then '+'; else c)

-- Truncate function that rounds doubles to specified number of decimal places, from StackOverflow answer
-- http://stackoverflow.com/a/31952975
truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

-- Helper function to hide printing the stops based on geolocation given, very crude and brute force due to
-- inability to loop through the JSON object given from parser
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
