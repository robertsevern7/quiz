{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.PubQuiz where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getPubQuizR :: Int -> QuestionType ->  Handler RepHtml
getPubQuizR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType randomPubQuiz (PubQuizR next questionType)

