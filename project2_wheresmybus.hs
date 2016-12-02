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
import qualified Data.ByteString.Lazy.Char8 as C
import Text.XML.Light
import Data.Char
import Control.Concurrent
import Control.Monad
import System.Exit

testurl = "http://maps.googleapis.com/maps/api/geocode/json?address=2366+Main+Mall,+Vancouver,+BC"
key' = "cURsrZ7AjbHgfC8cuZLV"
geocachekey = "AIzaSyAhSWqsa_BRpEJtM5FGhSa3ZGJjQ0FWByg"
url = "http://api.translink.ca/rttiapi/v1/stops/"
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
		
  else do
	let queryurl = url ++ bstop ++ "/" ++ estimates ++ routeno ++ broute
	let Just uri = parseURI queryurl	
	print uri
	test <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
	let jone = read test :: JSON
	-- let val = [jsq| jone[0].Code |]
	-- print val
	-- if (null val) then do
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
	-- else putStrLn "No estimates found for your request"

main2 = do
	putStrLn "Something"
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
