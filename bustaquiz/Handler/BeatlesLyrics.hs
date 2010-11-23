{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.BeatlesLyrics where

import Quiz
import Handler.Generic (genericRoute)

getBeatlesLyricsR :: Int -> Handler RepHtml
getBeatlesLyricsR seed = genericRoute seed beatlesLyrics (BeatlesLyricsR (succ seed))
    