{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.BeatlesLyrics where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getBeatlesLyricsR :: Int -> QuestionType -> Handler RepHtml
getBeatlesLyricsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType beatlesLyrics (BeatlesLyricsR next questionType)
    