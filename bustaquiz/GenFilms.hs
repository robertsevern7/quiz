module GenFilms where 

import Freebase (runQuery,wrapInQuery,mkObject)
import Control.Monad (liftM,forM)
import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Char8 as B

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
         (B.pack "value", Scalar JsonNull)
      ]
  ])]

{-
  {
    "id": null,
    "limit": 5000,
    "tagline": [
      {
        "optional": false,
        "value": null
      }
    ],
    "type": "/film/film"
  }
-}
filmsWithTaglines :: JsonObject
filmsWithTaglines = wrapInQuery $ toJsonObject $ Mapping [
  (B.pack "id", Scalar JsonNull),
  -- 5000 gave an empty response
  (B.pack "limit", Scalar $ JsonNumber 4000),
  (B.pack "type", Scalar $ toJsonScalar "/film/film"),
  (B.pack "tagline", Sequence [
      toJsonObject $ Mapping [
         (B.pack "optional", Scalar $ JsonBoolean False),
         (B.pack "value", Scalar JsonNull)
      ]
  ])]      
  
  {-
  {
    "!pd:/award/ranked_list/ranked_list_items": [
      {
        "!index": null,
        "id": "/en/the_movie_list_the_first_9200",
        "type": "/award/ranked_list"
      }
    ],
    "item": [
      {
        "id": null
      }
    ],
    "limit": 60,
    "sort": "!pd:/award/ranked_list/ranked_list_items.!index",
    "type": "/award/ranking"
  }
  -}
topFilms :: JsonObject
topFilms = wrapInQuery $ toJsonObject $ Mapping [
  (B.pack "type", Scalar $ toJsonScalar "/award/ranking"),
  (B.pack "limit", Scalar $ JsonNumber 3000),
  (B.pack "sort", Scalar $ toJsonScalar "!pd:/award/ranked_list/ranked_list_items.!index"),
  (B.pack "!pd:/award/ranked_list/ranked_list_items", Sequence [
      toJsonObject $ Mapping [
         (B.pack "!index", Scalar JsonNull),
         (B.pack "id", Scalar $ toJsonScalar "/en/the_movie_list_the_first_9200"),
         (B.pack "type", Scalar $ toJsonScalar "/award/ranked_list")
      ]
  ]),
  (B.pack "item", Sequence [
      toJsonObject $ Mapping [
         (B.pack "id", Scalar JsonNull)
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
  result' <- mapM fromMapping elements
  values <- mapM (lookupObject (B.pack "value")) result'
  z <- mapM fromScalar values
  return $ map fromJsonScalar z
  
saveActorsToDisk :: IO ()
saveActorsToDisk = do
   actors <- getActors
   writeFile actorPath (show actors)

getBigBudgetFilms :: String -> IO [(String, [Object B.ByteString JsonScalar])]
getBigBudgetFilms queryType = do
   let query = getBigBudgetFilmsQuery queryType
   queryResult <- runQueryAndGetResult query
   unmapped <- mapM fromMapping queryResult
   forM unmapped (\x -> do
            filmId <- lookupObject (B.pack "id") x >>= fromScalar
            films <- lookupObject (B.pack "film") x >>= fromSequence
            unmappedFilms <- mapM fromMapping films
            budgets <- mapM (lookupObject (B.pack "estimated_budget")) unmappedFilms
            return (fromJsonScalar filmId :: String,budgets))

