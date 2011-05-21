{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.MovieQuotes where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getQuoteSelectionR :: Int -> QuestionType ->  Handler RepHtml
getQuoteSelectionR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType quoteSelection (QuoteSelectionR next questionType)