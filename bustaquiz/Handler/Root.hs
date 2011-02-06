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
-- 3) Get rid of loginpanel

getIndex :: IO Int
getIndex = getStdRandom (randomR (1,2000000))

getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")
        setTitle "bustaquiz homepage"
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "homepage")

getWordplayR :: Handler RepHtml
getWordplayR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "wordplay")
        
getGeographyR :: Handler RepHtml
getGeographyR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "geography")
        
getMusicR :: Handler RepHtml
getMusicR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")        
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "music")

getFilmR :: Handler RepHtml
getFilmR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")        
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "film")

getHistoryR :: Handler RepHtml
getHistoryR = do
    mu <- maybeAuth
    defaultLayout $ do      
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "history")
        
getPubQuizR :: Handler RepHtml
getPubQuizR = do
    mu <- maybeAuth
    defaultLayout $ do      
        h2id <- newIdent
        index <- liftIO getIndex
        let loginpanel = $(hamletFile "loginpanel")
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "pubquiz")