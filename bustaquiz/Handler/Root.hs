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

-- TODO Remove boilerplate crap!

getIndex :: IO Int
getIndex = getStdRandom (randomR (1,2000000))

getRootR :: Handler RepHtml
getRootR = defaultLayout $ do
  index <- liftIO getIndex
  setTitle "bustaquiz homepage"
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "homepage")

getAcknowledgementsR :: Handler RepHtml
getAcknowledgementsR = defaultLayout $ do
  setTitle "bustaquiz Acknowledgements"
  addWidget $(hamletFile "acknowledgements")

getWordplayR :: Handler RepHtml
getWordplayR = defaultLayout $ do
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "wordplay")
        
getGeographyR :: Handler RepHtml
getGeographyR = defaultLayout $ do
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "geography")
        
getMusicR :: Handler RepHtml
getMusicR = defaultLayout $ do
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "music")

getFilmR :: Handler RepHtml
getFilmR = defaultLayout $ do
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "film")

getHistoryR :: Handler RepHtml
getHistoryR = defaultLayout $ do      
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "history")
        
getPubQuizR :: Handler RepHtml
getPubQuizR = defaultLayout $ do      
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "pubquiz")