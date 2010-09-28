module Films (
               WhichDirector
             , mkWhichDirector
             , FilmListDirectorQM
             , WhichActor
             , mkWhichActor
             , FilmListActorQM
             ) where

import Logic
import Freebase
import JsonHelper (lookupValue,getJSValue,getString,mkPath,mkIndex)

import System.Random
import Text.JSON
import Text.JSON.Types
import Data.List (sortBy)
import Data.Maybe (fromJust,mapMaybe,listToMaybe)
import Control.Monad

import Debug.Trace

data WhichDirector = WhichDirector [String]
data WhichActor = WhichActor [String]

-- TODO These guys haven't been tested since the great reshuffle.
data FilmListDirectorQM = FilmListDirectorQM [String]
data FilmListActorQM = FilmListActorQM

mkWhichDirector :: IO WhichDirector
mkWhichDirector = liftM WhichDirector readDirectorsFromDisk

instance QuestionMaker WhichDirector where
    generateQuestion (WhichDirector directors) = do
                                       films <- getDirectorFilmList =<< chooseFromList directors
                                       return $ generateQuestionWhoMadeThese "Who directed the following films?" films

instance QuestionMaker FilmListDirectorQM where
    generateQuestion _ = do
        films <- getDirectorFilmList directorPath
        return $ generateQuestionNameTheFilm films

mkWhichActor :: IO WhichActor
mkWhichActor = liftM WhichActor readActorsFromDisk

instance QuestionMaker WhichActor where
    generateQuestion (WhichActor films) = do
                                    actors <- getActorFilmList =<< chooseFromList films
                                    return $ generateQuestionWhoMadeThese "Who starred in the following films?" actors

instance QuestionMaker FilmListActorQM where
    generateQuestion _ = do
      films <- getActorFilmList actorPath
      return $ generateQuestionNameTheFilm films

directorPath :: FilePath
directorPath = "film/film_directors.txt"

actorPath :: FilePath
actorPath = "film/film_actors.txt"

listLimit:: JSValue
listLimit = JSRational False 10

generateQuestionWhoMadeThese :: String -> (String,Result [String]) -> Question
generateQuestionWhoMadeThese question (director, Ok films) = Question question (IdentifyFrom films director)

generateQuestionNameTheFilm :: (String,Result [String]) -> Question
generateQuestionNameTheFilm (name, Ok films) = Question ("Name as many films by " ++ name ++ " as possible.") (MultipleFreeText films)
generateQuestionNameTheFilm (_, Error err) = error $ "Failed to generate name the film: " ++ show err

getBigBudgetFilms :: String -> IO (Result [(String, Int)])
getBigBudgetFilms query_type = do
  let budgetQueryObject = showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])
      filmQueryObject = showJSON (toJSObject [("name", JSNull), ("limit", showJSON (5 :: Int)), ("sort", showJSON "-estimated_budget.amount"), ("estimated_budget", budgetQueryObject)]);
  response <- runQuery $ mkSimpleQuery [("type",showJSON query_type),("id",showJSON JSNull), ("limit",showJSON (600 :: Int)),("film", JSArray [filmQueryObject])]
  return (fmap extractIdAndBudgets $ lookupValue response "result")

extractIdAndBudgets :: JSValue -> [(String,Int)]
extractIdAndBudgets (JSArray xs) = sortBy (\(_,a) (_,b) -> compare b a) $ map extractIdAndBudget xs
extractIdAndBudgets _ = error "Freebase screwed us."

extractIdAndBudget :: JSValue -> (String,Int)
extractIdAndBudget (JSObject s) = (_id, extractBudget $ fromJust $ get_field s "film")
    where
      _id = getString (fromJust $ get_field s "id")
extractIdAndBudget _ = undefined

extractBudget :: JSValue -> Int
extractBudget (JSArray films) = sum $ map getFilmBudget films

-- There are two possible paths - try both and pick the one that works
getFilmBudget :: JSValue -> Int
getFilmBudget f@(JSObject _) = truncate cost
    where
      paths = [map mkPath ["film","estimated_budget","amount"],map mkPath ["estimated_budget","amount"]]
      (JSRational _ cost) = fromJust $ listToMaybe $ mapMaybe (getJSValue f) paths

getDirectorBigBudgetFilms :: IO (Result [(String, Int)])
getDirectorBigBudgetFilms = getBigBudgetFilms "/film/director"

{- The query for actors
[{
  "/people/person/profession": [
      {
        "id": "/en/actor",
        "type": "/people/profession"
      }
    ]
  "heat" : {"value": null, "optional": false},
  "id": [{
    "type": "/type/id",
    "value": null
  }],
  "type": "/base/popstra/celebrity",
  "limit": 10,
  "sort": "-heat.value"
}]
-}
getActors :: IO (Result [String])
getActors = do
  let professionFilter = JSArray[showJSON (toJSObject [("id", "/en/actor"), ("type", "/people/profession")])]
      heatObject = showJSON (toJSObject [("value", JSNull), ("optional", JSBool False)]);
      idObject = JSArray [showJSON (toJSObject [("type", showJSON "/type/id"), ("value", JSNull)])];
  response <- runQuery $ mkSimpleQuery [("type",showJSON "/base/popstra/celebrity")
                                       ,("/people/person/profession", professionFilter)
                                       ,("heat", heatObject)
                                       ,("limit",showJSON (200 :: Int))
                                       ,("id", idObject)
                                       ,("sort", showJSON "-heat.value")]
  let arrayActors = lookupValue response "result" 
  return (fmap extractActors arrayActors)
  
extractActors :: JSValue -> [String]
extractActors (JSArray xs) = map extractActor xs
extractActors _ = error "Freebase screwed us."

extractActor :: JSValue -> String
extractActor jsValue = getString (fromJust $ getJSValue jsValue [mkPath "id", mkIndex 0, mkPath "value"])

saveDirectorsToDisk :: IO ()
saveDirectorsToDisk = do
  (Ok films) <- getDirectorBigBudgetFilms
  writeFile directorPath (show $ map fst films)

readDirectorsFromDisk :: IO [String]
readDirectorsFromDisk = liftM read (readFile directorPath)

-- TODO 99?
getDirector :: IO String
getDirector = do
  directors <- readDirectorsFromDisk
  gen <- newStdGen
  let (i,_) = randomR (0,99) gen
  return (directors !! i)

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

saveActorsToDisk :: IO ()
saveActorsToDisk = do
  (Ok films) <- getActors
  writeFile actorPath (show $ map id films)

readActorsFromDisk :: IO [String]
readActorsFromDisk = liftM read (readFile actorPath)

getActor :: String -> IO String
getActor path = do
  actors <- readActorsFromDisk
  gen <- newStdGen
  let (i,_) = randomR (0,20) gen
  return (actors !! i)

