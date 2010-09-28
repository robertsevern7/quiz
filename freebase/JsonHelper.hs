module JsonHelper (
                   lookupValue
                  ,getFirst
                  ,getString
                  ,getJSValue
                  ,mkPath
                  ,mkIndex
                  ) where

import Text.JSON
import Text.JSON.Types
import Data.List ((!!))

data PathSegment = NamedField String
                 | ArrayIndex Int

mkPath :: String -> PathSegment
mkPath = NamedField 

mkIndex :: Int -> PathSegment
mkIndex = ArrayIndex

-- Should this use fmap?
lookupValue :: JSON a => Result JSValue -> String -> Result a
lookupValue (Ok (JSObject o)) key = valFromObj key o
lookupValue _ _                   = Error "Unsupported JSON response" 

getFirst :: Result JSValue -> Result JSValue
getFirst (Ok (JSArray (x:xs))) = Ok x
getFirst _ = Error "Not an array"

-- Get JS values by following a path indexing into fields as required
{-
getJSValue :: JSValue -> [String] -> Maybe JSValue
getJSValue j p = getJSValue' (Just j) p
    where
      getJSValue' Nothing _                    = Nothing
      getJSValue' (Just jsvalue) []            = Just jsvalue
      getJSValue' (Just (JSObject obj)) (x:xs) = getJSValue' (get_field obj x) xs -}

getJSValue :: JSValue -> [PathSegment] -> Maybe JSValue
getJSValue j p = getJSValue' (Just j) p
    where
      getJSValue' Nothing _ = Nothing
      getJSValue' (Just jsValue) [] = Just jsValue
      getJSValue' (Just (JSArray array)) ((ArrayIndex x):xs) | x <= (length array - 1) = getJSValue' (Just (array !! x)) xs
                                                             | otherwise = getJSValue' Nothing xs
      getJSValue' (Just (JSObject obj)) ((NamedField x):xs) = getJSValue' (get_field obj x) xs
      getJSValue' _ _ = Nothing

-- Get the string value or error
getString :: JSValue -> String
getString (JSString x) = fromJSString x
getString x = error $ "No string found when expected. =" ++ show x
