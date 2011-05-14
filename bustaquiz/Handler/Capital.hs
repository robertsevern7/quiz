{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getCapitalsR :: Int -> QuestionType -> Handler RepHtml
getCapitalsR seed questionType = genericRoute seed questionType whichCapital (\x -> CapitalsR x questionType)


