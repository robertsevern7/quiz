{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.MovieQuotes where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getQuoteSelectionR :: Int -> QuestionType ->  Handler RepHtml
getQuoteSelectionR seed questionType = genericRoute seed questionType quoteSelection (\x -> QuoteSelectionR x questionType)