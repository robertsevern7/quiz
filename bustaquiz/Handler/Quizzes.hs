module Handler.Quizzes where

import Quiz
import Logic (QuestionType,QuestionMaker)
import Handler.Generic (genericRoute)

getFiveLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getFiveLetterAnagramsR seed questionType = genericRoute seed questionType fiveLetter (`FiveLetterAnagramsR` questionType)

getSixLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSixLetterAnagramsR seed questionType = genericRoute seed questionType sixLetter (`SixLetterAnagramsR` questionType)

getSevenLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSevenLetterAnagramsR seed questionType = genericRoute seed questionType sevenLetter (`SevenLetterAnagramsR` questionType)

getEightLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getEightLetterAnagramsR seed questionType = genericRoute seed questionType eightLetter (`EightLetterAnagramsR` questionType)

getRandomPubQuizR :: Int -> QuestionType ->  Handler RepHtml
getRandomPubQuizR seed questionType = genericRoute seed questionType randomPubQuiz (`RandomPubQuizR` questionType)

getQuoteSelectionR :: Int -> QuestionType ->  Handler RepHtml
getQuoteSelectionR seed questionType = genericRoute seed questionType quoteSelection (`QuoteSelectionR` questionType)

getStateFlagsR :: Int -> QuestionType -> Handler RepHtml
getStateFlagsR seed questionType = genericRoute seed questionType stateFlags (`StateFlagsR` questionType)

getUSPresidentsOrderR :: Int -> QuestionType ->  Handler RepHtml
getUSPresidentsOrderR seed questionType = genericRoute seed questionType presidentOrder (`USPresidentsOrderR` questionType)

getTaglinesR :: Int -> QuestionType ->  Handler RepHtml
getTaglinesR seed questionType = genericRoute seed questionType filmTaglines (`TaglinesR` questionType)

getCountryFlagsR :: Int -> QuestionType -> Handler RepHtml
getCountryFlagsR seed questionType = genericRoute seed questionType countryFlags (`CountryFlagsR` questionType)

getCapitalsR :: Int -> QuestionType -> Handler RepHtml
getCapitalsR seed questionType = genericRoute seed questionType whichCapital (`CapitalsR` questionType)

getBeatlesLyricsR :: Int -> QuestionType -> Handler RepHtml
getBeatlesLyricsR seed questionType = genericRoute seed questionType beatlesLyrics (`BeatlesLyricsR` questionType)
    
