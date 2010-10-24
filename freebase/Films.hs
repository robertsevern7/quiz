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
import JsonHelper (lookupValue,getJSValue,getString,mkPath,mkIndex)

import System.Random
import Text.JSON
import Text.JSON.Types
import Data.List (sortBy)
import Data.Maybe (fromJust,mapMaybe,listToMaybe)
import Data.Array.MArray
import Control.Monad
import Control.Monad.Trans (liftIO)

import Debug.Trace

debug :: Show a => a -> a
debug x = trace (show x) x

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
	  
directorPath :: FilePath
directorPath = "film/film_directors.txt"

actorPath :: FilePath
actorPath = "film/film_actors.txt"

filmPath :: FilePath
filmPath = "film/film_films.txt"

listLimit:: JSValue
listLimit = JSRational False 10

generateQuestionWhoMadeThese :: String -> (String,Result [String]) -> Question
generateQuestionWhoMadeThese question (director, Ok films) = Question question (IdentifyFrom films director)

generateQuestionNameTheFilm :: (String,Result [String]) -> Question
generateQuestionNameTheFilm (name, Ok films) = Question ("Name as many films by " ++ name ++ " as possible.") (MultipleFreeText films)
generateQuestionNameTheFilm (_, Error err) = error $ "Failed to generate name the film: " ++ show err

generateQuestionFilmTaglines :: Result [(String,String)] -> Question
generateQuestionFilmTaglines (Ok filmTaglines) = Question "Name the films from their taglines" (Identify filmTaglines)
generateQuestionFilmTaglines (Error err) = error $ "Failed to generate film taglines: " ++ show err

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
extractIdAndBudget jsValue = (getString (fromJust $ getJSValue jsValue [mkPath "id"])
                             ,extractBudget $ fromJust $ getJSValue jsValue [mkPath "film"])

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

getDirector :: IO String
getDirector = do
  directors <- readDirectorsFromDisk
  gen <- newStdGen
  let (i,_) = randomR (0,min 99 $ length directors) gen
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

saveFilmsToDisk :: IO ()
saveFilmsToDisk = do
  (Ok films) <- getFilms
  writeFile filmPath (show $ map id films)

{- The query for actors
[{
  "!pd:/award/ranked_list/ranked_list_items": [{
    "!index": null,
    "id": "/en/the_movie_list_the_first_9200",
    "type": "/award/ranked_list"
  }],   
  "item": [{
    "tagline": [{
      "value": null
    }],
    "type": "/film/film"
  }],
  "limit": 1,
  "rank": null,
  "sort": "!pd:/award/ranked_list/ranked_list_items.!index",
  "summary:item": [{
    "id": null,
    "optional": false,
    "type": "/award/ranked_item"
  }],
  "type": "/award/ranking"
}]

​
-}
getFilms :: IO (Result [String])
getFilms = do
  let taglineObject = JSArray[showJSON (toJSObject [("value", JSNull)])]
      rankedListObject = JSArray[showJSON (toJSObject [("!index", JSNull), ("id", showJSON "/en/the_movie_list_the_first_9200"), ("type", showJSON "/award/ranked_list")])]
      itemObject = showJSON (toJSObject [("tagline", taglineObject), ("type", showJSON "/film/film")]);
      summaryItemObject = JSArray[showJSON (toJSObject [("id", JSNull), ("optional", JSBool False), ("type", showJSON "/award/ranked_item")])];
  response <- runQuery $ mkSimpleQuery [("!pd:/award/ranked_list/ranked_list_items", rankedListObject)
                                       ,("item", itemObject)
                                       ,("summary:item", summaryItemObject)
                                       ,("limit",showJSON (500 :: Int))
                                       ,("rank", JSNull)
                                       ,("sort", showJSON "!pd:/award/ranked_list/ranked_list_items.!index")
									   ,("type", showJSON "/award/ranking")]
  let arrayFilms = lookupValue response "result" 
  return (fmap extractFilms arrayFilms)
  
extractFilms :: JSValue -> [String]
extractFilms (JSArray xs) = map extractFilm xs
extractFilms _ = error "Freebase screwed us."

extractFilm :: JSValue -> String
extractFilm jsValue = getString (fromJust $ getJSValue jsValue [mkPath "summary:item", mkIndex 0, mkPath "id"])

getActor :: String -> IO String
getActor path = do
  actors <- readActorsFromDisk
  gen <- newStdGen
  let (i,_) = randomR (0,min 99 $ length actors) gen
  return (actors !! i)
  
readFilmsFromDisk :: IO [String]
readFilmsFromDisk = liftM read (readFile filmPath)
  
rnd_select xs n 
    | n < 0     = error "N must be greater than zero."
    | otherwise = replicateM n rand
        where rand = do r <- randomRIO (0, (length xs) - 1)
                        return (xs!!r)
						
getTaglineFilmPairs :: JSValue -> [(String, String)]						
getTaglineFilmPairs (JSArray xs) = map getTaglineFilmPair xs
						
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
  