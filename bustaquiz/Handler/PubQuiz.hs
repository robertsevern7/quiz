{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.PubQuiz where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getRandomPubQuizR :: Int -> QuestionType ->  Handler RepHtml
getRandomPubQuizR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType randomPubQuiz (RandomPubQuizR next questionType)

