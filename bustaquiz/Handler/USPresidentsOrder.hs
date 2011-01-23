{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.USPresidentsOrder where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getUSPresidentsOrderR :: Int -> QuestionType ->  Handler RepHtml
getUSPresidentsOrderR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType presidentOrder (USPresidentsOrderR next questionType)


