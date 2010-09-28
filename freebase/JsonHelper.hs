module JsonHelper (
                   lookupValue
                  ,getFirst
                  ,getString
                  ) where

import Text.JSON

-- Should this use fmap?
lookupValue :: JSON a => Result JSValue -> String -> Result a
lookupValue (Ok (JSObject o)) key = valFromObj key o
lookupValue _ _                   = Error "Unsupported JSON response" 

getFirst :: Result JSValue -> Result JSValue
getFirst (Ok (JSArray (x:xs))) = Ok x
getFirst _ = Error "Not an array"

getString :: Result JSValue -> String
getString (Ok (JSString x)) = fromJSString x
getString _ = error "No string found when expected."