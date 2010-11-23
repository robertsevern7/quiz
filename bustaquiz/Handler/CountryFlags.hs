{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.CountryFlags where

import Quiz
import Handler.Generic (genericRoute)

getCountryFlagsR :: Int -> Handler RepHtml
getCountryFlagsR seed = genericRoute seed countryFlags (CountryFlagsR (succ seed))