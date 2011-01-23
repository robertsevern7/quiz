{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Taglines where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getTaglinesR :: Int -> QuestionType ->  Handler RepHtml
getTaglinesR seed questionType = genericRoute seed questionType filmTaglines (TaglinesR (succ seed) questionType)

