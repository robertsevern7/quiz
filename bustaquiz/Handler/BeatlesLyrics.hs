{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.BeatlesLyrics where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getBeatlesLyricsR :: Int -> QuestionType -> Handler RepHtml
getBeatlesLyricsR seed questionType= genericRoute seed questionType beatlesLyrics (BeatlesLyricsR (succ seed) questionType)
    