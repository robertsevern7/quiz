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
                                             

