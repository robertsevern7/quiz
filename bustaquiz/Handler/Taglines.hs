{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Taglines where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getTaglinesR :: Int -> QuestionType ->  Handler RepHtml
getTaglinesR seed questionType = genericRoute seed questionType filmTaglines (\x -> TaglinesR x questionType)

