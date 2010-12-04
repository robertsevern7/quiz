{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Anagrams where

import Quiz
import Handler.Generic (genericRoute)

getFiveLetterAnagramsR :: Int -> Handler RepHtml
getFiveLetterAnagramsR seed = genericRoute seed fiveLetter (FiveLetterAnagramsR (succ seed))

getSixLetterAnagramsR :: Int -> Handler RepHtml
getSixLetterAnagramsR seed = genericRoute seed sixLetter (SixLetterAnagramsR (succ seed))

getSevenLetterAnagramsR :: Int -> Handler RepHtml
getSevenLetterAnagramsR seed = genericRoute seed sevenLetter (SevenLetterAnagramsR (succ seed))

getEightLetterAnagramsR :: Int -> Handler RepHtml
getEightLetterAnagramsR seed = genericRoute seed eightLetter (EightLetterAnagramsR (succ seed))