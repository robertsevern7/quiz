{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz (Widget,QuizRoute)
import Settings (hamletFile)


runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
-- Display the question as a widget
questionTemplate :: QuizRoute -> Question -> Widget ()
questionTemplate route (Question description (Associate pairs)) = undefined  -- $(hamletFile "associate")
questionTemplate _ (Question _ _) = error "This has not been implemented yet."


