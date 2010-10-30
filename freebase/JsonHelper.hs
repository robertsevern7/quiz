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

-- TODO syntax for this could be greatly simplified
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
getFirst (Ok (JSArray (x:_))) = Ok x
getFirst _ = Error "Not an array"

getJSValue :: JSValue -> [PathSegment] -> Maybe JSValue
getJSValue j p = getJSValue' (Just j) p
    where
      getJSValue' Nothing _ = Nothing
      getJSValue' (Just jsValue) [] = Just jsValue
      getJSValue' (Just (JSArray array)) (ArrayIndex x:xs) | x <= (length array - 1) = getJSValue' (Just (array !! x)) xs
                                                           | otherwise = getJSValue' Nothing xs
      getJSValue' (Just (JSObject obj)) (NamedField x:xs) = getJSValue' (get_field obj x) xs
      getJSValue' _ _ = Nothing

-- Get the string value or error
getString :: JSValue -> String
getString (JSString x) = fromJSString x
getString x = error $ "No string found when expected. =" ++ show x
