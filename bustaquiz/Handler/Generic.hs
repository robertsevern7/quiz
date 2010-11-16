{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion,
  questionWidget) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz (Widget,QuizRoute,StaticRoute)
import Settings (cassiusFile,hamletFile,juliusFile,jqueryURL)
import Yesod.Widget (addCassius,addHamlet,addJulius,addScriptRemote)
import Yesod.Helpers.Static
import StaticFiles
import Data.Either

runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
-- Display the question as a widget
questionWidget :: QuizRoute -> Question -> Widget ()
questionWidget route (Question description (Associate pairs)) = do
  -- External requirements
  addScriptRemote jqueryURL
  addJulius $(juliusFile "shuffle")
  addJulius $(juliusFile "text")
  
  -- Actual code
  addHamlet $(hamletFile "associate")
  addCassius $(cassiusFile "associate")
  addJulius $(juliusFile "associate")
  
questionWidget _ (Question _ _) = error "This has not been implemented yet."


