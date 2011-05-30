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
  let quizmenu = $(hamletFile "quizMenu")
  setTitle "bustaquiz homepage"
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "homepage")
  addJulius $(juliusFile "quizMenu")

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
  let quizmenu = $(hamletFile "quizMenu")
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "geography")
  addJulius $(juliusFile "quizMenu")
        
getMusicR :: Handler RepHtml
getMusicR = defaultLayout $ do
  let quizmenu = $(hamletFile "quizMenu")
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "music")
  addJulius $(juliusFile "quizMenu")

getFilmR :: Handler RepHtml
getFilmR = defaultLayout $ do
  let quizmenu = $(hamletFile "quizMenu")
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "film")
  addJulius $(juliusFile "quizMenu")

getHistoryR :: Handler RepHtml
getHistoryR = defaultLayout $ do      
  let quizmenu = $(hamletFile "quizMenu")
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "history")
  addJulius $(juliusFile "quizMenu")
  
getPubQuizR :: Handler RepHtml
getPubQuizR = defaultLayout $ do      
  let quizmenu = $(hamletFile "quizMenu")
  index <- liftIO getIndex
  addCassius $(cassiusFile "homepage")
  addWidget $(hamletFile "pubquiz")
  addJulius $(juliusFile "quizMenu")