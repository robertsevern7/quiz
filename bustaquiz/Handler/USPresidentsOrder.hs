{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.USPresidentsOrder where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getUSPresidentsOrderR :: Int -> QuestionType ->  Handler RepHtml
getUSPresidentsOrderR seed questionType = genericRoute seed questionType presidentOrder (\x -> USPresidentsOrderR x questionType)


