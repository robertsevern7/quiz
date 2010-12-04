{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Anagrams where

import Quiz
import Handler.Generic (genericRoute)

getSixLetterAnagramsR :: Int -> Handler RepHtml
getSixLetterAnagramsR seed = genericRoute seed sixLetter (SixLetterAnagramsR (succ seed))