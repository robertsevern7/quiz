{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.CountryFlags where

import Quiz
import Logic (QuestionType) 
import Handler.Generic (genericRoute)
import System.Random

getCountryFlagsR :: Int -> QuestionType -> Handler RepHtml
getCountryFlagsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType countryFlags (CountryFlagsR next questionType)