{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Anagrams where

import Quiz
import Handler.Generic (genericRoute)

getAnagramsR :: Int -> Handler RepHtml
getAnagramsR seed = genericRoute seed anagrams (AnagramsR (succ seed))