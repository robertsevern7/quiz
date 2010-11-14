{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion,
  questionWidget) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz (Widget,QuizRoute)
import Settings (hamletFile)
import Yesod.Widget (addHamlet)

runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
-- Display the question as a widget
questionWidget :: QuizRoute -> Question -> Widget ()
questionWidget route (Question description (Associate pairs)) = do
  addHamlet $(hamletFile "associate")
questionWidget _ (Question _ _) = error "This has not been implemented yet."


