{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.USPresidentsOrder where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getUSPresidentsOrderR :: Int -> QuestionType ->  Handler RepHtml
getUSPresidentsOrderR seed questionType = genericRoute seed questionType presidentOrder (USPresidentsOrderR (succ seed) questionType)


