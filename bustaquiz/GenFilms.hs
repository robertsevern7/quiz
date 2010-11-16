module GenFilms where 
{-(
   saveDirectorsToDisk,
   saveActorsToDisk,
   saveFilmsToDisk,
   readDirectorsFromDisk,
   readFilmsFromDisk,
   readActorsFromDisk
   ) where -}

import Freebase (runQuery, mkSimpleQuery,wrapInQuery,mkObject)
-- import JsonHelper (lookupValue,getString,mkPath,mkIndex,getJSValue)
import Data.List (sortBy)
import Control.Monad (liftM)
import Data.Maybe (fromJust,mapMaybe,listToMaybe)

import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Char8 as B
import Data.Attempt
import Data.Convertible.Base

directorPath :: FilePath
directorPath = "data/film/film_directors.txt"

actorPath :: FilePath
actorPath = "data/film/film_actors.txt"

filmPath :: FilePath
filmPath = "data/film/film_films.txt"

readDirectorsFromDisk :: IO [String]
readDirectorsFromDisk = liftM read (readFile directorPath)

readActorsFromDisk :: IO [String]
readActorsFromDisk = liftM read (readFile actorPath)

readFilmsFromDisk :: IO [String]
readFilmsFromDisk = liftM read (readFile filmPath)

-- TODO remove unnecessary packing!
getActorsQuery :: JsonObject
getActorsQuery = wrapInQuery $ toJsonObject $ Mapping [
  (B.pack "/people/person/profession", mkObject [
      ("id", toJsonScalar "/en/actor"),
      ("type", toJsonScalar "/people/profession")
      ]),
  (B.pack "heat", mkObject [
      ("value", JsonNull),
      ("optional", JsonBoolean False) 
      ]),
  (B.pack "id",Sequence [mkObject [("type", toJsonScalar "/type/id"),("value", JsonNull)]]),
  (B.pack "type", Scalar $ toJsonScalar "/base/popstra/celebrity"),
  (B.pack "limit", Scalar $ JsonNumber 10), -- TODO update the limit
  (B.pack "sort", Scalar $ toJsonScalar "-heat.value")]
  
getActors :: IO [String]
getActors = do
  queryResult <- runQuery getActorsQuery >>= fromMapping 
  result <- lookupObject (B.pack "result") queryResult >>= fromSequence
  unmapped <- mapM fromMapping result
  elements <- mapM (\a -> fmap head (lookupObject (B.pack "id") a >>= fromSequence)) unmapped
  result <- mapM fromMapping elements
  values <- mapM (lookupObject (B.pack "value")) result  
  z <- mapM fromScalar values
  return $ map fromJsonScalar z
  
saveActorsToDisk :: IO ()
saveActorsToDisk = do
   actors <- getActors
   writeFile actorPath (show actors)

getBigBudgetFilmsQuery :: String -> JsonObject
getBigBudgetFilmsQuery queryType = wrapInQuery $ toJsonObject $ Mapping [
  (B.pack "type", Scalar $ toJsonScalar queryType),
  (B.pack "id", Scalar $ JsonNull),
  (B.pack "limit", Scalar $ JsonNumber 600),
  (B.pack "film", Sequence [
      toJsonObject $ Mapping [
         (B.pack "name", Scalar JsonNull),
         (B.pack "limit", Scalar $ JsonNumber 5),
         (B.pack "sort", Scalar $ toJsonScalar "-estimated_budget.amount"),
         (B.pack "estimated_budget", toJsonObject $ Mapping [
             (B.pack "amount", Scalar JsonNull),
             (B.pack "currency", Scalar $ toJsonScalar "US$")])
      ]
  ])]


-- getBigBudgetFilms :: String -> IO (Result [(String, Int)])
getBigBudgetFilms query_type = do
   let query = getBigBudgetFilmsQuery query_type
   queryResult <- runQuery query >>= fromMapping    
   return queryResult

   {-
   --       filmQueryObject = showJSON (toJSObject [("name", JSNull), ("limit", showJSON (5 :: Int)), ("sort", showJSON "-estimated_budget.amount"), ("estimated_budget", budgetQueryObject)]);
--   response <- runQuery $ mkSimpleQuery [("type",showJSON query_type),("id",showJSON JSNull), ("limit",showJSON (600 :: Int)),("film", JSArray [filmQueryObject])]
--   return (fmap extractIdAndBudgets $ lookupValue response "result")
-}

-- extractIdAndBudgets :: JSValue -> [(String,Int)]
-- extractIdAndBudgets (JSArray xs) = sortBy (\(_,a) (_,b) -> compare b a) $ map extractIdAndBudget xs
-- extractIdAndBudgets _ = error "Freebase screwed us."

-- extractIdAndBudget :: JSValue -> (String,Int)
-- extractIdAndBudget jsValue = (getString (fromJust $ getJSValue jsValue [mkPath "id"])
--                              ,extractBudget $ fromJust $ getJSValue jsValue [mkPath "film"])

-- extractBudget :: JSValue -> Int
-- extractBudget (JSArray films) = sum $ map getFilmBudget films
-- extractBudget _ = error "Unexpected response in extractBudget"

-- -- There are two possible paths - try both and pick the one that works
-- getFilmBudget :: JSValue -> Int
-- getFilmBudget f@(JSObject _) = truncate cost
--     where
--       paths = [map mkPath ["film","estimated_budget","amount"],map mkPath ["estimated_budget","amount"]]
--       (JSRational _ cost) = fromJust $ listToMaybe $ mapMaybe (getJSValue f) paths
-- getFilmBudget _ = error "Unexpected response in getFilmBudget"

--getDirectorBigBudgetFilms :: IO (Result [(String, Int)])
getDirectorBigBudgetFilms = getBigBudgetFilms "/film/director"

-- saveDirectorsToDisk :: IO ()
-- saveDirectorsToDisk = do
--   (Ok films) <- getDirectorBigBudgetFilms
--   writeFile directorPath (show $ map fst films)

-- {- The query for actors
-- [{
--   "!pd:/award/ranked_list/ranked_list_items": [{
--     "!index": null,
--     "id": "/en/the_movie_list_the_first_9200",
--     "type": "/award/ranked_list"
--   }],   
--   "item": [{
--     "tagline": [{
--       "value": null
--     }],
--     "type": "/film/film"
--   }],
--   "limit": 1,
--   "rank": null,
--   "sort": "!pd:/award/ranked_list/ranked_list_items.!index",
--   "summary:item": [{
--     "id": null,
--     "optional": false,
--     "type": "/award/ranked_item"
--   }],
--   "type": "/award/ranking"
-- }]

-- ​
-- -}
-- getFilms :: IO (Result [String])
-- getFilms = do
--   let taglineObject = JSArray[showJSON (toJSObject [("value", JSNull)])]
--       rankedListObject = JSArray[showJSON (toJSObject [("!index", JSNull), ("id", showJSON "/en/the_movie_list_the_first_9200"), ("type", showJSON "/award/ranked_list")])]
--       itemObject = showJSON (toJSObject [("tagline", taglineObject), ("type", showJSON "/film/film")]);
--       summaryItemObject = JSArray[showJSON (toJSObject [("id", JSNull), ("optional", JSBool False), ("type", showJSON "/award/ranked_item")])];
--   response <- runQuery $ mkSimpleQuery [("!pd:/award/ranked_list/ranked_list_items", rankedListObject)
--                                        ,("item", itemObject)
--                                        ,("summary:item", summaryItemObject)
--                                        ,("limit",showJSON (500 :: Int))
--                                        ,("rank", JSNull)
--                                        ,("sort", showJSON "!pd:/award/ranked_list/ranked_list_items.!index")
-- 									   ,("type", showJSON "/award/ranking")]
--   let arrayFilms = lookupValue response "result" 
--   return (fmap extractFilms arrayFilms)
  
-- extractFilms :: JSValue -> [String]
-- extractFilms (JSArray xs) = map extractFilm xs
-- extractFilms _ = error "Freebase screwed us."

-- extractFilm :: JSValue -> String
-- extractFilm jsValue = getString (fromJust $ getJSValue jsValue [mkPath "summary:item", mkIndex 0, mkPath "id"])

-- saveFilmsToDisk :: IO ()
-- saveFilmsToDisk = do
--   (Ok films) <- getFilms
--   writeFile filmPath (show $ map id films)
-- -}