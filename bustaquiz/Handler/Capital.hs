{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Quiz
import Handler.Generic (runQuestion)

getCapitalsR :: Int -> Handler RepHtml
getCapitalsR seed = do
  quiz <- getYesod
  question <- liftIO $ runQuestion seed (whichCapital quiz)
  defaultLayout $ do
    setTitle "Give the capital cities as quick as you can."
  

