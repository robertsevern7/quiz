module Freebase (
                 mkSimpleQuery
                ,wrapInQuery
--                ,runSimpleQuery
                ,runQuery
                ,touch
                ,status
                ,version
                ,simpleService
                ,mkObject
                ) where

import Network.HTTP
import Control.Monad

import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Char8 as B

import Debug.Trace

touch :: String
touch = "http://api.freebase.com/api/service/touch" 

status :: String
status = "http://api.freebase.com/api/status" 

version :: String
version = "http://api.freebase.com/api/version" 

simpleService :: String -> IO JsonObject
simpleService s = do
  a <- (simpleHTTP (getRequest s) >>= getResponseBody)
  decode (B.pack a)

mqlReadUri :: String
mqlReadUri = "http://api.freebase.com/api/service/mqlread"

runQuery :: JsonObject -> IO JsonObject
runQuery s = do
  let request = traceShow (getRequest (mqlReadUri ++ "?query=" ++ (urlEncode . B.unpack) (encode s))) (getRequest (mqlReadUri ++ "?query=" ++ (urlEncode . B.unpack) (encode s)))
  a <- simpleHTTP request >>= getResponseBody
  decode (B.pack a)
  
mkObject :: [(String,JsonScalar)] -> JsonObject
mkObject x = Mapping ((map (\(a,b) -> (B.pack a, Scalar b))) x)

wrapInQuery :: JsonObject -> JsonObject
wrapInQuery x = toJsonObject $ Mapping [(B.pack "query", Sequence [x])]

mkSimpleQuery :: [(String,JsonScalar)] -> JsonObject
mkSimpleQuery = wrapInQuery . mkObject

-- runSimpleQuery :: String -> String -> String -> JsonScalar -> (JsonScalar -> String) -> IO (String, Result [String])
-- runSimpleQuery qtype key id_ arr extractor = do
--   response <- runQuery $ mkSimpleQuery [("type",toJsonScalar qtype),("id",toJsonScalar id_), ("name", JsonNull),(key,arr)]
--   return response
--   {-
--   let k = lookupValue response "result"
--       (Ok name) = lookupValue (getFirst k) "name"
--       l = lookupValue (getFirst k) key
--       m = fmap (\(JSArray x) -> x) l
--   return (getString name, fmap (map extractor) m)


