{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.StateFlags where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getStateFlagsR :: Int -> QuestionType -> Handler RepHtml
getStateFlagsR seed questionType = do
  randomNext <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType stateFlags (StateFlagsR randomNext questionType)