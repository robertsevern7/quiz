{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Root where

import Quiz
import Logic
import System.Random (getStdRandom, randomR)

-- TODO Remove boilerplate crap!  Slightly difficult due to GHC stage restriction
-- and template Haskell

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