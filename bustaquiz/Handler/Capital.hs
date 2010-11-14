{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Control.Exception (try,evaluate)
import Logic
import Exception

import Quiz

getCapitalsR :: Int -> Handler RepHtml
getCapitalsR seed = undefined

-- TODO all following this should go into a generic method --

runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
getQuestionSource :: QuestionMaker a => (Quiz -> a) -> Int -> QuizRoute -> Handler RepHtml
getQuestionSource getQuestion seed route = do
  quizMaster <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed (getQuestion quizMaster)
  case generatedQuestion of
    (Left ex)        -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right question) -> undefined
    
    {-
      let body = (questionTemplate route question)
          additionalJS = (jsSource question)
          questions = $(hamletFileDebug "templates/bodybase.hamlet")
      defaultLayout $ do
        addHamletHead (headTemplate additionalJS)
        addHamlet  questions-}
