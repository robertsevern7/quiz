{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.StateFlags where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getStateFlagsR :: Int -> QuestionType -> Handler RepHtml
getStateFlagsR seed questionType = genericRoute seed questionType stateFlags (\x -> StateFlagsR x questionType)