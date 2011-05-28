{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Quiz
import Logic

import System.Random

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Quiz.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

-- TODO 
-- 1) Get rid of index <- liftIO getIndex
-- 2) Get rid of Bustaquiz

getIndex :: IO Int
getIndex = getStdRandom (randomR (1,2000000))

getRootR :: Handler RepHtml
getRootR = do
    defaultLayout $ do
        index <- liftIO getIndex
        setTitle "bustaquiz homepage"
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "homepage")
        addJulius $(juliusFile "quizMenu")

getAcknowledgementsR :: Handler RepHtml
getAcknowledgementsR = do
  defaultLayout $ do
    setTitle "bustaquiz Acknowledgements"
    addWidget $(hamletFile "acknowledgements")

getWordplayR :: Handler RepHtml
getWordplayR = do
    defaultLayout $ do
        index <- liftIO getIndex
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "wordplay")
        
getGeographyR :: Handler RepHtml
getGeographyR = do
    defaultLayout $ do
        index <- liftIO getIndex
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "geography")
        
getMusicR :: Handler RepHtml
getMusicR = do
    defaultLayout $ do
        index <- liftIO getIndex
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "music")

getFilmR :: Handler RepHtml
getFilmR = do
    defaultLayout $ do
        index <- liftIO getIndex
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "film")

getHistoryR :: Handler RepHtml
getHistoryR = do
    defaultLayout $ do      
        index <- liftIO getIndex
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "history")
        
getPubQuizR :: Handler RepHtml
getPubQuizR = do
    defaultLayout $ do      
        index <- liftIO getIndex
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "pubquiz")