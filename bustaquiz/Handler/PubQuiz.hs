{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.PubQuiz where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getRandomPubQuizR :: Int -> QuestionType ->  Handler RepHtml
getRandomPubQuizR seed questionType = genericRoute seed questionType randomPubQuiz (\x -> RandomPubQuizR x questionType)

