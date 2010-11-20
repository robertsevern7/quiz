{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Quiz
import Handler.Generic (genericRoute)

getCapitalsR :: Int -> Handler RepHtml
getCapitalsR seed = genericRoute seed whichCapital (CapitalsR (succ seed))


