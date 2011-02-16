{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Taglines where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getTaglinesR :: Int -> QuestionType ->  Handler RepHtml
getTaglinesR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType filmTaglines (TaglinesR next questionType)

