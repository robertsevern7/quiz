{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.USPresidentsOrder where

import Quiz
import Handler.Generic (runQuestion,questionWidget)
import Exception(message,internal)

getUSPresidentsOrderR :: Int -> Handler RepHtml
getUSPresidentsOrderR seed = do
  quiz <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed (presidentOrder quiz)
  case generatedQuestion of
    (Left ex) -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right question) -> defaultLayout $ do
      setTitle "Give the capital cities as quick as you can."
      addWidget (questionWidget (CapitalsR (succ seed)) question)


