{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getCapitalsR :: Int -> QuestionType -> Handler RepHtml
getCapitalsR seed questionType = genericRoute seed questionType whichCapital (CapitalsR (succ seed) questionType)


