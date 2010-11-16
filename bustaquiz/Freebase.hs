module Freebase (
                 mkSimpleQuery
--                ,runSimpleQuery
                ,runQuery
                ,touch
                ,status
                ,version
                ,simpleService
                ) where

import Network.HTTP
import Control.Monad

import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Char8 as B

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
  a <- simpleHTTP (getRequest (mqlReadUri ++ "?query=" ++ (urlEncode . B.unpack) (encode s))) >>= getResponseBody
  decode (B.pack a)

mkSimpleQuery :: [(String,JsonScalar)] -> JsonObject
mkSimpleQuery x = toJsonObject $ Mapping (map (\(a,b) -> (B.pack a,Scalar b)) x)


{-
runSimpleQuery :: String -> String -> String -> JSValue -> (JSValue -> String) -> IO (String, Result [String])
runSimpleQuery qtype key id_ arr extractor = do
  response <- runQuery $ mkSimpleQuery [("type",showJSON qtype),("id",showJSON id_), ("name", JSNull),(key,arr)]
  let k = lookupValue response "result"
      (Ok name) = lookupValue (getFirst k) "name"
      l = lookupValue (getFirst k) key
      m = fmap (\(JSArray x) -> x) l
  return (getString name, fmap (map extractor) m)
-}