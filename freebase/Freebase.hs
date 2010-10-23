module Freebase (
                 mkSimpleQuery
                ,runSimpleQuery
                ,runQuery
                ) where

import Text.JSON
import JsonHelper

import Network.HTTP
import Network.URI (URI,parseURI)
import Control.Monad
import Data.Maybe (fromJust)

touch :: String
touch = "http://api.freebase.com/api/service/touch" 

status :: String
status = "http://api.freebase.com/api/status" 

version :: String
version = "http://api.freebase.com/api/version" 

simpleService :: String -> IO (Result JSValue)
simpleService s = liftM decode (simpleHTTP (getRequest s) >>= getResponseBody)

mqlReadUri :: String
mqlReadUri = "http://api.freebase.com/api/service/mqlread"

runQuery :: JSValue -> IO (Result JSValue)
runQuery s = liftM decode (simpleHTTP (getRequest (mqlReadUri ++ "?query=" ++ urlEncode (encode s))) >>= getResponseBody) 

mkSimpleQuery :: [(String,JSValue)] -> JSValue
mkSimpleQuery x = JSObject $ toJSObject [("query", JSArray [JSObject $ toJSObject x])]

runSimpleQuery :: String -> String -> String -> JSValue -> (JSValue -> String) -> IO (String, Result [String])
runSimpleQuery qtype key id_ arr extractor = do
  response <- runQuery $ mkSimpleQuery [("type",showJSON qtype),("id",showJSON id_), ("name", JSNull),(key,arr)]
  let k = lookupValue response "result"
      (Ok name) = lookupValue (getFirst k) "name"
      l = lookupValue (getFirst k) key
      m = fmap (\(JSArray x) -> x) l
  return (getString name, fmap (map extractor) m)
