{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.USPresidentsOrder where

import Quiz
import Handler.Generic (genericRoute)

getUSPresidentsOrderR :: Int -> Handler RepHtml
getUSPresidentsOrderR seed = genericRoute seed presidentOrder (USPresidentsOrderR (succ seed))


