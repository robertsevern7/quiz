{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withQuiz
    ) where

import Quiz hiding (beatlesLyrics,stateFlags,sixLetter,sevenLetter,filmTaglines,randomPubQuiz,quoteSelection)
import Settings
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Database.Persist.GenericSql

-- Relevant handlers
import Handler.Root
import Handler.Anagrams
import Handler.Capital
import Handler.BeatlesLyrics
import Handler.USPresidentsOrder
import Handler.StateFlags
import Handler.CountryFlags
import Handler.Taglines
import Handler.PubQuiz
import Handler.MovieQuotes

-- Quiz makers
import Anagram (fiveLetterAnagrams, sixLetterAnagrams, sevenLetterAnagrams, eightLetterAnagrams)
import Country (capitalQuiz,countryFlagsQuiz)
import Lyrics (beatlesLyrics)
import Presidents (orderOfService)
import States (stateFlags)
import PubQuiz (randomPubQuiz)
import MovieQuotes (quoteSelection)

-- TODO declarations
import Taglines (filmTaglines)

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Quiz.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Quiz" resourcesQuiz

-- TODO Customize the FavIcon
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

-- TODO Customize the Robots.txt file 
getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent "User-agent: *"

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withQuiz :: (Application -> IO a) -> IO a
withQuiz f = Settings.withConnectionPool $ \pool -> do
    runConnectionPool (runMigration migrateAll) pool
    -- TODO This all seems a bit nasty - perhaps centralizing this in the data base is a better idea?
    -- TODO Or at least introducing a separate object!
    filmTaglines' <- filmTaglines
    let h = Quiz s pool capitalQuiz countryFlagsQuiz beatlesLyrics orderOfService stateFlags fiveLetterAnagrams sixLetterAnagrams sevenLetterAnagrams eightLetterAnagrams filmTaglines' randomPubQuiz quoteSelection
    toWaiApp h >>= f
  where
    s = fileLookupDir Settings.staticdir typeByExt
