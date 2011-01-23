module GenFilms where 

import Freebase (runQuery, mkSimpleQuery,wrapInQuery,mkObject)
import Data.List (sortBy)
import Control.Monad (liftM,forM)
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
                 
getBigBudgetFilmsQuery :: String -> JsonObject
getBigBudgetFilmsQuery queryType = wrapInQuery $ toJsonObject $ Mapping [
  (B.pack "type", Scalar $ toJsonScalar queryType),
  (B.pack "id", Scalar JsonNull),
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
                                   
tagLineQuery :: [String] -> JsonObject                                   
tagLineQuery films = wrapInQuery $ toJsonObject $ Mapping [
  (B.pack "id|=", Sequence $ map (Scalar . toJsonScalar) films),
  (B.pack "name", Scalar JsonNull),
  (B.pack "type", Scalar $ toJsonScalar "/film/film"),
  (B.pack "tagline", Sequence [
      toJsonObject $ Mapping [
         (B.pack "limit", Scalar $ JsonNumber 1),
         (B.pack "value", Scalar $ JsonNull)
      ]
  ])]


runQueryAndGetResult :: JsonObject -> IO [Object B.ByteString JsonScalar]
runQueryAndGetResult query = do
  queryResult <- runQuery query >>= fromMapping
  lookupObject (B.pack "result") queryResult >>= fromSequence

getActors :: IO [String]
getActors = do
  result <- runQueryAndGetResult getActorsQuery
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

-- TODO sum films if we need to
-- getBigBudgetFilms :: String -> IO [(String, Int)] -- [Object B.ByteString JsonScalar])]
getBigBudgetFilms queryType = do
   let query = getBigBudgetFilmsQuery queryType
   queryResult <- runQueryAndGetResult query
   unmapped <- mapM fromMapping queryResult
--   return queryResult 
   forM unmapped (\x -> do
            filmId <- lookupObject (B.pack "id") x >>= fromScalar
            films <- lookupObject (B.pack "film") x >>= fromSequence
            unmappedFilms <- mapM fromMapping films
            budgets <- mapM (lookupObject (B.pack "estimated_budget")) unmappedFilms
            unmappedBudgets <- mapM fromMapping films            
            return (fromJsonScalar filmId :: String,budgets))

