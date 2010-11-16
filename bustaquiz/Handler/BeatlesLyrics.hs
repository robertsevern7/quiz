{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.BeatlesLyrics where

import Quiz
import Handler.Generic (runQuestion,questionWidget)
import Exception (message,internal)
import Logic (description)

getBeatlesLyricsR seed = do
  quiz <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed (beatlesLyrics quiz)
  case generatedQuestion of
    (Left ex) -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right question) -> defaultLayout $ do
      setTitle "Identify the Beatles songs given a snippet of the lyrics"
      addWidget (questionWidget (BeatlesLyricsR (succ seed)) question)
    