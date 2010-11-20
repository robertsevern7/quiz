{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.StateFlags where

import Quiz
import Handler.Generic (genericRoute)

getStateFlagsR :: Int -> Handler RepHtml
getStateFlagsR seed = genericRoute seed stateFlags (StateFlagsR (succ seed))