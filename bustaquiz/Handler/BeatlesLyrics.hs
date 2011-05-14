{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.BeatlesLyrics where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getBeatlesLyricsR :: Int -> QuestionType -> Handler RepHtml
getBeatlesLyricsR seed questionType = genericRoute seed questionType beatlesLyrics (\x -> BeatlesLyricsR x questionType)
    