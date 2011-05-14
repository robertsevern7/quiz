{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.CountryFlags where

import Quiz
import Logic (QuestionType) 
import Handler.Generic (genericRoute)
import System.Random

getCountryFlagsR :: Int -> QuestionType -> Handler RepHtml
getCountryFlagsR seed questionType = genericRoute seed questionType countryFlags (`CountryFlagsR` questionType)