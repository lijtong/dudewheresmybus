-- !!! COPIED
module JSON where

import Data.Char
import Data.Map hiding (map)
import Text.ParserCombinators.Parsec hiding (token)

-- COPIED END

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP
import Network.HTTP.Headers
import Network.URI
import qualified Data.ByteString.Lazy.Char8 as C
import Text.XML.Light
import Data.Char

key' = "cURsrZ7AjbHgfC8cuZLV"
url = "http://api.translink.ca/rttiapi/v1/stops/52026/estimates?apikey=" ++ key'
Just uri = parseURI url
header' = Header HdrAccept "application/JSON"

request = simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody

-- COPIED
-- !!! remove all this
--------------------------------------------------------------------------

data JsonValue = JsonString String
           | JsonNumber Double
           | JsonObject (Map String JsonValue)
           | JsonArray  [JsonValue]
           | JsonTrue
           | JsonFalse
           | JsonNull
  deriving (Show, Eq)

--------------------------------------------------------------------------
-- Convenient parse combinators

token :: Parser a -> Parser a
token p = do r <- p
             spaces
             return r

comma :: Parser Char
comma = token (char ',')

--------------------------------------------------------------------------

parseJSON :: String -> JsonValue
parseJSON str = case (parse jsonFile "" str) of
               Left  s -> error (show s)
               Right v -> v

jsonFile :: Parser JsonValue
jsonFile = do contents <- jsonObject <|> jsonArray
              eof
              return contents

-- JSON Object
jsonObject :: Parser JsonValue
jsonObject = do pairs <- between open close (sepBy jsonPair comma)
                return $ JsonObject $ fromList pairs
  where
  open  = token (char '{')
  close = token (char '}')

jsonPair :: Parser (String, JsonValue)
jsonPair = do key   <- token(jsonString)
              token (char ':')
              value <- token(jsonValue)
              return (toString key, value)
  where
 toString (JsonString s) = s
 toString _ = ""

-- JSON Array
jsonArray :: Parser JsonValue
jsonArray = do values <- between open close (sepBy (token jsonValue) comma)
               return $ JsonArray values
  where
 open  = token (char '[')
 close = token (char ']')


-- Any JSON Value
jsonValue :: Parser JsonValue
jsonValue = do spaces
               obj <- token(jsonString
                             <|> jsonNumber
                             <|> jsonObject
                             <|> jsonArray
                             <|> jsonTrue
                             <|> jsonFalse
                             <|> jsonNull
                             )
               return obj

-- JSON String
jsonString :: Parser JsonValue
jsonString = do s <- between (char '"' ) (char '"' ) (many jsonChar)
                return (JsonString s)

isValidJsonChar ch = (isAscii ch) && (isPrint ch) && (ch /= '\\') && (ch /= '"')

hexToInt s = Prelude.foldl (\i j -> (16 * i) + j) 0 (map digitToInt s)

jsonChar = satisfy isValidJsonChar
           <|> do char '\\'  -- escaping backslash
                  char '\\'  -- escaped character
                    <|> char '"'
                    <|> char '/'
                    <|> (char 'b' >> return '\b')
                    <|> (char 'f' >> return '\f')
                    <|> (char 'n' >> return '\n')
                    <|> (char 'r' >> return '\r')
                    <|> (char 't' >> return '\t')
                    <|> do char 'u'
                           hex <- count 4 (satisfy isHexDigit)
                           return $ chr (hexToInt hex)

-- JSON Number
jsonNumber :: Parser JsonValue
jsonNumber = do i    <- int
                frac <- option "" frac
                e    <- option "" expo
                return $ JsonNumber (read (i ++ frac ++ e))

int :: Parser String
int = do sign  <- option "" (string "-")
         value <- (string "0" <|> many1 digit)
         return (sign ++ value)

frac :: Parser String
frac = do char '.'
          digits <- many1 digit
          return ( '.':digits)

expo :: Parser String
expo = do e <- oneOf "eE"
          p <- option '+' (oneOf "+-")
          n <- many1 digit
          return (e : p : n)


-- JSON Constants
jsonTrue  = token (string "true")  >> return JsonTrue
jsonFalse = token (string "false") >> return JsonFalse
jsonNull  = token (string "null")  >> return JsonNull

--------------------------------------------------------------------------
-- A JSON Pretty Printer
--------------------------------------------------------------------------
pprint v = toString "" v

toString indent (JsonString s) = show s
toString indent (JsonNumber d) = show d
toString indent (JsonObject o) =
 if (o == empty)
  then "{}"
  else "{\n" ++ showObjs  (indent ++ " ") (toList o) ++ "\n" ++ indent ++ "}"
toString indent (JsonArray []) = "[]"
toString indent (JsonArray a)  = "[\n" ++ showArray (indent ++ " ") a          ++ "\n" ++ indent ++ "]"
toString indent (JsonTrue)     = "true"
toString indent (JsonFalse)    = "false"
toString indent (JsonNull)     = "null"

showKeyValue i k v    = i ++ show k ++ ": " ++ toString i v

showObjs i []         = ""
showObjs i [(k ,v)]   = showKeyValue i k v
showObjs i ((k, v):t) = showKeyValue i k v ++ ",\n" ++ showObjs i t

showArray i []    = ""
showArray i [a]   = i ++ toString i a
showArray i (h:t) = i ++ toString i h ++ ",\n" ++ showArray i t

-- !!!
-- COPIED END


-- preface: program runs in terms of ACTIONS and do blocks.
--            as one action is finished, you link it to another action.

-- note: the last line of "do" blocks are required to be "Actions"; i.e. we can't just output a number or string. CTRL-F and see the last line with "!!!". Our code won't compile without this.

-- note: This feels scraggily, or weak. I'm wondering if there's another way to go about this. This is a start, however.

-- first action called. Will give introductory information before action 1 is called.
-- !!!
main = do
  putStrLn "Hello! Welcome to where's my bus! We'll get you your bus schedule."
  test <- simpleHTTP (Request uri GET [header'] "") >>= fmap (Prelude.take 10000) . getResponseBody
  -- !!!! CURRENTLY, test acts as a string.
  putStrLn test
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

------------------------------
-- Helper functions belong below, so as to make our "Actions" more clear and simple.

-- take string. trim string. Check only if it's 5 digits and all numbers.
isValidBus :: Foldable t => t Char -> Bool
isValidBus xs = (all isDigit xs) && (isValidLength xs)

-- check if string's length is 5.
isValidLength :: Foldable t => t a -> Bool
isValidLength xs = length xs == 5




-- http://api.translink.ca/RTTIAPI/V1/stops/55612?apiKey=cURsrZ7AjbHgfC8cuZLV
