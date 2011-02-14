{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getCapitalsR :: Int -> QuestionType -> Handler RepHtml
getCapitalsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType whichCapital (CapitalsR next questionType)


