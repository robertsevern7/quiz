module JsonHelper (
                   lookupValue
                  ,getFirst
                  ,getString
                  ,getJSValue
                  ) where

import Text.JSON
import Text.JSON.Types

-- Should this use fmap?
lookupValue :: JSON a => Result JSValue -> String -> Result a
lookupValue (Ok (JSObject o)) key = valFromObj key o
lookupValue _ _                   = Error "Unsupported JSON response" 

getFirst :: Result JSValue -> Result JSValue
getFirst (Ok (JSArray (x:xs))) = Ok x
getFirst _ = Error "Not an array"

{- Boring helper functions -}
-- Get JS values by following a path indexing into fields as required
getJSValue :: JSValue -> [String] -> Maybe JSValue
getJSValue j p = getJSValue' (Just j) p
    where
      getJSValue' Nothing _                    = Nothing
      getJSValue' (Just jsvalue) []            = Just jsvalue
      getJSValue' (Just (JSObject obj)) (x:xs) = getJSValue' (get_field obj x) xs

getString :: JSValue -> String
getString (JSString x) = fromJSString x
getString x = error $ "No string found when expected. =" ++ show x
