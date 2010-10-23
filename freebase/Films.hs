module Films (
               WhichDirector
             , mkWhichDirector
             , WhichActor
             , mkWhichActor
             , WhichFilm
             , mkWhichFilm
             ) where

import Logic
import Freebase
import GenFilms (readDirectorsFromDisk,readActorsFromDisk,readFilmsFromDisk)
import JsonHelper (lookupValue,getJSValue,getString,mkPath,mkIndex)

import System.Random
import Text.JSON
import Data.Maybe (fromJust)
import Control.Monad

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
    generateQuestion (WhichDirector directors) = do
                                       films <- getDirectorFilmList =<< chooseFromList directors
                                       return $ generateQuestionWhoMadeThese "Who directed the following films?" films

instance QuestionMaker WhichActor where
    generateQuestion (WhichActor films) = do
                                    actors <- getActorFilmList =<< chooseFromList films
                                    return $ generateQuestionWhoMadeThese "Who starred in the following films?" actors
									
									--this is not code
instance QuestionMaker WhichFilm where
    generateQuestion (WhichFilm films) = do
                                    (Ok tagLines) <- getTaglineFilmList =<< rnd_select films 10
                                    return $ Question "Name the films from the taglines" (Identify tagLines)
	  
listLimit:: JSValue
listLimit = JSRational False 10

generateQuestionWhoMadeThese :: String -> (String,Result [String]) -> Question
generateQuestionWhoMadeThese question (director, Ok films) = Question question (IdentifyFrom films director)
generateQuestionWhoMadeThese _ (_, Error err) = error $ "Failed to generate question " ++ err

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

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n 
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)
						
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
  