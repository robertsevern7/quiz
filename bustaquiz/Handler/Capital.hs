{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Capital where

import Control.Exception (try,evaluate)
import Logic
import Exception

import Quiz
import Handler.Generic

getCapitalsR :: Int -> Handler RepHtml
getCapitalsR seed = undefined

