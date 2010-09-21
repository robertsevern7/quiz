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
import Debug.Trace

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

runQuery :: JSValue -> IO (Result JSValue)
runQuery s = liftM decode (simpleHTTP (getRequest (mqlReadUri ++ "?query=" ++ urlEncode (encode s))) >>= getResponseBody) 

mkSimpleQuery :: [(String,JSValue)] -> JSValue
mkSimpleQuery x = JSObject $ toJSObject [("query", JSArray [JSObject $ toJSObject x])]

runSimpleQuery :: String -> String -> String -> JSValue -> (JSValue -> String) -> IO (String,(Result [String]))
runSimpleQuery qtype key name arr extractor = do
  response <- runQuery $ mkSimpleQuery [("type",showJSON qtype),("id",showJSON name), ("name", JSNull),(key,arr)]
  let k = lookupValue response "result"
      l = lookupValue (getFirst k) key
      m = fmap (\(JSArray x) -> x) l
  return (name,fmap (map extractor) m)

