module Handler.Quizzes where

import Quiz
import Logic (QuestionType,QuestionMaker)
import Handler.Generic (genericRoute)

getFiveLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getFiveLetterAnagramsR = genericRoute fiveLetter FiveLetterAnagramsR

getSixLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSixLetterAnagramsR = genericRoute sixLetter SixLetterAnagramsR

getSevenLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSevenLetterAnagramsR = genericRoute sevenLetter SevenLetterAnagramsR

getEightLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getEightLetterAnagramsR = genericRoute eightLetter EightLetterAnagramsR

getRandomPubQuizR :: Int -> QuestionType ->  Handler RepHtml
getRandomPubQuizR = genericRoute randomPubQuiz RandomPubQuizR

getQuoteSelectionR :: Int -> QuestionType ->  Handler RepHtml
getQuoteSelectionR = genericRoute quoteSelection QuoteSelectionR

getStateFlagsR :: Int -> QuestionType -> Handler RepHtml
getStateFlagsR = genericRoute stateFlags StateFlagsR

getUSPresidentsOrderR :: Int -> QuestionType ->  Handler RepHtml
getUSPresidentsOrderR = genericRoute presidentOrder USPresidentsOrderR

getTaglinesR :: Int -> QuestionType ->  Handler RepHtml
getTaglinesR = genericRoute filmTaglines TaglinesR

getCountryFlagsR :: Int -> QuestionType -> Handler RepHtml
getCountryFlagsR = genericRoute countryFlags CountryFlagsR

getCapitalsR :: Int -> QuestionType -> Handler RepHtml
getCapitalsR = genericRoute whichCapital CapitalsR

getBeatlesLyricsR :: Int -> QuestionType -> Handler RepHtml
getBeatlesLyricsR = genericRoute beatlesLyrics BeatlesLyricsR
    
