module Films (
               WhichDirector
             , mkWhichDirector
             , WhichActor
             , mkWhichActor
             , WhichFilm
             , mkWhichFilm
             ) where

import Exception (QuizException(QuizException))
import Logic
import Freebase
import GenFilms (readDirectorsFromDisk,readActorsFromDisk,readFilmsFromDisk)
import JsonHelper (lookupValue,getJSValue,getString,mkPath,mkIndex)

import System.Random
import Text.JSON
import Data.Maybe (fromJust)
import Control.Monad (liftM,replicateM)
import Control.Exception (throw)
import Data.Char (toLower) 

-- These will be question makers supplied with the information
-- necessary to construct a question
data WhichDirector = WhichDirector [String]
data WhichActor = WhichActor [String]
data WhichFilm = WhichFilm [String]

--[Read in list from disk to memory]
mkWhichActor :: IO WhichActor
mkWhichActor = liftM WhichActor readActorsFromDisk

mkWhichDirector :: IO WhichDirector
mkWhichDirector = liftM WhichDirector readDirectorsFromDisk

mkWhichFilm :: IO WhichFilm
mkWhichFilm = liftM WhichFilm readFilmsFromDisk

--[Create QuestionMaker instances]
instance QuestionMaker WhichDirector where
    generateQuestion seed (WhichDirector directors) = do
      films <- getDirectorFilmList $ chooseFromList seed directors
      return $ generateQuestionWhoMadeThese "Who directed the following films?" films

instance QuestionMaker WhichActor where
    generateQuestion seed (WhichActor films) = do
      actors <- getActorFilmList $ chooseFromList seed films
      return $ generateQuestionWhoMadeThese "Who starred in the following films?" actors          
									
instance QuestionMaker WhichFilm where
    generateQuestion seed (WhichFilm films) = do
      (Ok tagLines) <- getTaglineFilmList =<< rndSelect seed films 10      
      return $ Question "Name the films from the taglines" (Identify $ hideFilmNames tagLines)
                       
listLimit:: JSValue
listLimit = JSRational False 10

hideFilmNames :: [(String,String)] -> [(String,String)]
hideFilmNames = map redact 

redact :: (String,String) -> (String,String)
redact (film,tagline) = (film,unwords $ replacer (words film) (words tagline))

replacer :: [String] -> [String] -> [String]
replacer movieWords = map (redactionReturner movieWords) 

redactionReturner :: [String] -> String -> String
redactionReturner movieWords taglineWord | tagWord `elem` stopWords = taglineWord
                                         | tagWord `elem` mw = "_____"
                                         | otherwise = taglineWord 
                                           where
                                             mw = map lower movieWords
                                             tagWord = lower taglineWord

generateQuestionWhoMadeThese :: String -> (String,Result [String]) -> Question
generateQuestionWhoMadeThese question (director, Ok films) = Question question (IdentifyFrom films director)
generateQuestionWhoMadeThese _ (_, Error err) = throw $ QuizException "Failed to generate question " err

getDirectorFilmList :: String -> IO (String,Result [String])
getDirectorFilmList director = runSimpleQuery "/film/director" "film" director (JSArray [filmQueryObject]) (extract ["name"])
    where
      filmQueryObject = showJSON (toJSObject [("estimated_budget", showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])), ("name", JSNull), ("limit", listLimit), ("sort", showJSON "-estimated_budget.amount")])

extract :: [String] -> JSValue -> String
extract path jsValue = getString $ fromJust $ getJSValue jsValue (map mkPath path)

getActorFilmList :: String -> IO (String,Result [String])
getActorFilmList actor = runSimpleQuery "/film/actor" "film" actor (JSArray [filmQueryObject]) (extract ["film","name"])
    where
      filmQueryObject = showJSON $ toJSObject [("film", filmObj)
                                              ,("limit", listLimit)
                                              ,("sort", showJSON "-film.estimated_budget.amount")]
      filmObj = showJSON (toJSObject [("estimated_budget", showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])), ("name", JSNull)])
	
getTaglineFilmPairs :: JSValue -> [(String, String)]						
getTaglineFilmPairs (JSArray xs) = map getTaglineFilmPair xs
getTaglineFilmPairs _ = error "Unexpected type in getTaglineFilmPairs"
						
getTaglineFilmPair :: JSValue -> (String, String)
getTaglineFilmPair filmTagJs = (getString (fromJust $ getJSValue filmTagJs [mkPath "name"]), tagline)
  where
    tagline = getString (fromJust $ getJSValue filmTagJs [mkPath "tagline", mkIndex 0, mkPath "value"])

getTaglineFilmList :: [String] -> IO (Result [(String, String)])
getTaglineFilmList filmIds = do
  let filmJSValues = map showJSON filmIds
      taglineObj = JSArray[showJSON (toJSObject [("value", JSNull), ("limit", showJSON (1 :: Int))])]
  response <- runQuery $ mkSimpleQuery [("type",showJSON "/film/film"),("id|=",JSArray filmJSValues), ("name", JSNull),("tagline",taglineObj)]
  let arrayFilmsAndTags = lookupValue response "result"
  return (fmap getTaglineFilmPairs arrayFilmsAndTags)
  
lower :: String -> String
lower = map toLower

stopWords :: [String]
stopWords = ["the","be","to","of","and","a","in","that","have","i","it","for","not","on","with","he","as","you","do","at","this","but","his","by","from","they","we","say","her","she","or","an","will","my","one","all","would","there","their","what","so","up","out","if","about","who","get","which","go","me","when","make","can","like","no","just","him","know","take","into","year","your","some","could","them","see","other","than","then","now","look","only","come","its","over","think","also","back","after","use","two","how","our","work","first","well","way","even","new","want","because","any","these","give","day","most","us"]