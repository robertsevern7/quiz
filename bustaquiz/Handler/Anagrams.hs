{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Anagrams where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)
import System.Random

getFiveLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getFiveLetterAnagramsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType fiveLetter (FiveLetterAnagramsR next questionType)

getSixLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSixLetterAnagramsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))  
  genericRoute seed questionType sixLetter (SixLetterAnagramsR next questionType)

getSevenLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSevenLetterAnagramsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))  
  genericRoute seed questionType sevenLetter (SevenLetterAnagramsR next questionType)

getEightLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getEightLetterAnagramsR seed questionType = do
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  genericRoute seed questionType eightLetter (EightLetterAnagramsR next questionType)