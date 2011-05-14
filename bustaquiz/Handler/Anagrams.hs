{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Anagrams where

import Quiz
import Logic (QuestionType,QuestionMaker)
import Handler.Generic (genericRoute)
import System.Random

getFiveLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getFiveLetterAnagramsR seed questionType = genericRoute seed questionType fiveLetter (`FiveLetterAnagramsR` questionType)

getSixLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSixLetterAnagramsR seed questionType = genericRoute seed questionType sixLetter (`SixLetterAnagramsR` questionType)

getSevenLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSevenLetterAnagramsR seed questionType = genericRoute seed questionType sevenLetter (`SevenLetterAnagramsR` questionType)

getEightLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getEightLetterAnagramsR seed questionType = genericRoute seed questionType eightLetter (`EightLetterAnagramsR` questionType)