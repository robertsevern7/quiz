{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion,
  questionWidget) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz (Widget,QuizRoute)
import Settings (hamletFile,juliusFile,jqueryURL)
import Yesod.Widget (addHamlet,addJulius,addScriptRemote)

runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
-- Display the question as a widget
questionWidget :: QuizRoute -> Question -> Widget ()
questionWidget route (Question description (Associate pairs)) = do
  addHamlet $(hamletFile "associate")
  addScriptRemote jqueryURL -- TODO use addScriptEither and get local copy
  addJulius $(juliusFile "associate")
questionWidget _ (Question _ _) = error "This has not been implemented yet."


