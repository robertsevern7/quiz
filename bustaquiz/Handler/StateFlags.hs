{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.StateFlags where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getStateFlagsR :: Int -> QuestionType -> Handler RepHtml
getStateFlagsR seed questionType = genericRoute seed questionType stateFlags (StateFlagsR (succ seed) questionType)