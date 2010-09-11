module Freebase where

import System.Random
import Text.JSON
import Text.JSON.Types
import Network.HTTP
import Network.URI
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad

import Data.Maybe (fromJust)

getFirst :: Result JSValue -> Result JSValue
getFirst (Ok (JSArray (x:xs))) = Ok x
getFirst _ = Error "Not an array"

-- Should this use fmap?
lookupValue :: JSON a => Result JSValue -> String -> Result a
lookupValue (Ok (JSObject o)) key = valFromObj key o
lookupValue _ _                   = Error "Unsupported JSON response" 

touch :: URI
touch = fromJust $ parseURI "http://api.freebase.com/api/service/touch" 

status :: URI
status = fromJust $ parseURI "http://api.freebase.com/api/status" 

version :: URI
version = fromJust $ parseURI "http://api.freebase.com/api/version" 

simpleService :: URI -> IO (Result JSValue)
simpleService s = liftM decode (simpleHTTP (mkRequest GET s) >>= getResponseBody)

mqlReadUri :: String
mqlReadUri = "http://api.freebase.com/api/service/mqlread"

makeQuery :: JSValue -> IO (Result JSValue)
makeQuery s = liftM decode (simpleHTTP (getRequest (mqlReadUri ++ "?query=" ++ urlEncode (encode s))) >>= getResponseBody) 

mkSimpleQuery :: [(String,JSValue)] -> JSValue
mkSimpleQuery x = JSObject $ toJSObject [("query", JSArray [JSObject $ toJSObject x])]

runSimpleQuery :: String -> String -> String -> IO (String,(Result [String]))
runSimpleQuery qtype key name = do
  response <- makeQuery $ mkSimpleQuery [("type",showJSON qtype),("id",showJSON name),(key, JSArray [])]
  let k = lookupValue response "result"
      l = lookupValue (getFirst k) key
      m = fmap (\(JSArray x) -> x) l
  return (name,fmap (map (\(JSString x) -> fromJSString x)) m)

getAlbumList :: String -> IO (String,Result [String])
getAlbumList = runSimpleQuery "/music/artist" "album" 

-- don't hardcode, but easy for now!
listOfDirectors :: [String]
listOfDirectors = ["Blake Edwards",
                   "D. A. Pennebaker",
                   "Chris Hegedus",
                   "David Dawkins",
                   "Zack Snyder",
                   "Bernardo Bertolucci",
                   "Steven Spielberg",
                   "Michael Radford",
                   "Wong Kar-wai",
                   "John Landis",
                   "Luis Mandoki",
                   "Norman Jewison",
                   "Pedro Almodóvar",
                   "Francis Delia",
                   "Scott Norlund",
                   "Mark Osborne",
                   "Dror Soref",
                   "Robert K. Weiss",
                   "Hal Warren",
                   "Michael Dimich",
                   "Bharathiraja",
                   "Aparna Sen",
                   "Nathan Juran",
                   "George Seaton",
                   "John Singleton",
                   "Phil Karlson",
                   "Jean-Jacques Beineix",
                   "Joel Schumacher",
                   "Henry Hathaway",
                   "Richard Fleischer",
                   "Selvaraghavan",
                   "Kim Ki-duk",
                   "Delmer Daves"]