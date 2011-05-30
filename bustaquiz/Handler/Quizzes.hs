module Handler.Quizzes where

import Quiz
import Logic (QuestionType)
import Handler.Generic (genericRoute)

getFiveLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getFiveLetterAnagramsR index questionType = genericRoute fiveLetter FiveLetterAnagramsR index questionType "1"

getSixLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSixLetterAnagramsR index questionType = genericRoute sixLetter SixLetterAnagramsR index questionType "1"

getSevenLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getSevenLetterAnagramsR index questionType = genericRoute sevenLetter SevenLetterAnagramsR index questionType "1"

getEightLetterAnagramsR :: Int -> QuestionType -> Handler RepHtml
getEightLetterAnagramsR index questionType = genericRoute eightLetter EightLetterAnagramsR index questionType "1"

getRandomPubQuizR :: Int -> QuestionType ->  Handler RepHtml
getRandomPubQuizR index questionType = genericRoute randomPubQuiz RandomPubQuizR index questionType "2"

getQuoteSelectionR :: Int -> QuestionType ->  Handler RepHtml
getQuoteSelectionR index questionType = genericRoute quoteSelection QuoteSelectionR index questionType "2"

getStateFlagsR :: Int -> QuestionType -> Handler RepHtml
getStateFlagsR index questionType = genericRoute stateFlags StateFlagsR index questionType "2"

getUSPresidentsOrderR :: Int -> QuestionType ->  Handler RepHtml
getUSPresidentsOrderR index questionType = genericRoute presidentOrder USPresidentsOrderR index questionType "2"

getTaglinesR :: Int -> QuestionType ->  Handler RepHtml
getTaglinesR index questionType = genericRoute filmTaglines TaglinesR index questionType "2"

getCountryFlagsR :: Int -> QuestionType -> Handler RepHtml
getCountryFlagsR index questionType = genericRoute countryFlags CountryFlagsR index questionType "2"

getCapitalsR :: Int -> QuestionType -> Handler RepHtml
getCapitalsR index questionType = genericRoute whichCapital CapitalsR index questionType "2"

getBeatlesLyricsR :: Int -> QuestionType -> Handler RepHtml
getBeatlesLyricsR index questionType = genericRoute beatlesLyrics BeatlesLyricsR index questionType "2"