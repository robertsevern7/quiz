{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz
import Settings (hamletFile)


runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
-- Run the question, product
questionTemplate :: QuizRoute -> Question -> Hamlet (Route Quiz)
questionTemplate route (Question description (Associate pairs)) = $(hamletFile "associate")
questionTemplate _ (Question _ _) = error "This has not been implemented yet."


