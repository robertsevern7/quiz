{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Quiz
import Logic
-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- Quiz.hs; look for the line beginning with mkYesodData.
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle "bustaquiz homepage"
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "homepage")

getWordplayR :: Handler RepHtml
getWordplayR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "wordplay")
        
getGeographyR :: Handler RepHtml
getGeographyR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        addCassius $(cassiusFile "homepage")
        addWidget $(hamletFile "geography")