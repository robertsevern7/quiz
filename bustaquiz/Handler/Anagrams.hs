{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Anagrams where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getFiveLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getFiveLetterAnagramsR seed questionType = genericRoute seed questionType fiveLetter (FiveLetterAnagramsR (succ seed) questionType)

getSixLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSixLetterAnagramsR seed questionType = genericRoute seed questionType sixLetter (SixLetterAnagramsR (succ seed) questionType)

getSevenLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSevenLetterAnagramsR seed questionType = genericRoute seed questionType sevenLetter (SevenLetterAnagramsR (succ seed) questionType)

getEightLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getEightLetterAnagramsR seed questionType = genericRoute seed questionType eightLetter (EightLetterAnagramsR (succ seed) questionType)