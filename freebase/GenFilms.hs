module GenFilms (
   saveDirectorsToDisk,
   saveActorsToDisk,
   saveFilmsToDisk,
   readDirectorsFromDisk,
   readFilmsFromDisk,
   readActorsFromDisk
   ) where

import Text.JSON
import Freebase (runQuery, mkSimpleQuery)
import JsonHelper (lookupValue,getString,mkPath,mkIndex,getJSValue)
import Data.List (sortBy)
import Control.Monad (liftM)
import Data.Maybe (fromJust,mapMaybe,listToMaybe)

directorPath :: FilePath
directorPath = "film/film_directors.txt"

actorPath :: FilePath
actorPath = "film/film_actors.txt"

filmPath :: FilePath
filmPath = "film/film_films.txt"

readDirectorsFromDisk :: IO [String]
readDirectorsFromDisk = liftM read (readFile directorPath)

readActorsFromDisk :: IO [String]
readActorsFromDisk = liftM read (readFile actorPath)

readFilmsFromDisk :: IO [String]
readFilmsFromDisk = liftM read (readFile filmPath)


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

saveActorsToDisk :: IO ()
saveActorsToDisk = do
  (Ok films) <- getActors
  writeFile actorPath (show $ map id films)

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

saveDirectorsToDisk :: IO ()
saveDirectorsToDisk = do
  (Ok films) <- getDirectorBigBudgetFilms
  writeFile directorPath (show $ map fst films)

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

saveFilmsToDisk :: IO ()
saveFilmsToDisk = do
  (Ok films) <- getFilms
  writeFile filmPath (show $ map id films)
