module Films where

import System.Random
import Text.JSON
import Text.JSON.Types
import Network.HTTP
import Network.URI
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Maybe (isNothing)
import Control.Monad
import Freebase

import Data.Maybe (fromJust)
import Debug.Trace
import Logic

{- We should only need to expose the question makers from this module -}

data DirectorQuestionMaker = DirectorQuestionMaker {
      directors :: [String] -- |^Directors from which to choose questions
}

instance QuestionMaker DirectorQuestionMaker where
    generateQuestion _ = Question "Name as many films directed by Peter Jackson as you can"
                          (MultipleFreeText ["Film 1","Film 2","Film 3","Film 4"])
 
data ActorQuestionMaker = ActorQuestionMaker {
      actors :: [String] -- |^Actors from which to choose questions
}

-- TODO get rid of canned implementation
instance QuestionMaker ActorQuestionMaker where
    generateQuestion _ = Question "Name as many films starring Donald Sutherland as you can"
                          (MultipleFreeText ["Film 1","Film 2","Film 3","Film 4"])

directorPath :: FilePath
directorPath = "film/film_directors.txt"

actorPath :: FilePath
actorPath = "film/film_actors.txt"

getBigBudgetFilms :: String -> IO (Result [(String, Int)])
getBigBudgetFilms query_type = do
  let budgetQueryObject = showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])
      filmQueryObject = showJSON (toJSObject [("name", JSNull), ("limit", showJSON (5 :: Int)), ("sort", showJSON "-estimated_budget.amount"), ("estimated_budget", budgetQueryObject)]);
  response <- runQuery $ mkSimpleQuery [("type",showJSON query_type),("id",showJSON JSNull), ("limit",showJSON (600 :: Int)),("film", JSArray [filmQueryObject])]
  let arrayOfIdAndFilms = (lookupValue response "result" :: Result JSValue)
  return (fmap extractIdAndBudgets arrayOfIdAndFilms)
  
extractIdAndBudgets :: JSValue -> [(String,Int)]
extractIdAndBudgets (JSArray xs) = sortBy (\(_,a) (_,b) -> compare b a) $ map extractIdAndBudget xs
extractIdAndBudgets _ = error "Freebase screwed us."
  
extractIdAndBudget :: JSValue -> (String,Int)
extractIdAndBudget (JSObject s) = (_id, extractBudget $ fromJust $ get_field s "film")
    where
      _id = (\(JSString z) -> fromJSString z) (fromJust $ get_field s "id")
extractIdAndBudget _ = undefined

extractBudget :: JSValue -> Int
extractBudget (JSArray films) = sum $ map getFilmBudget films

getFilmBudget :: JSValue -> Int
getFilmBudget f@(JSObject film) = truncate cost
    where
      (JSObject filmObject) = getFilmObject f
      (JSObject estimatedBudget) = fromJust $ get_field filmObject "estimated_budget"
      (JSRational _ cost) = fromJust $ get_field estimatedBudget "amount"

getFilmObject :: JSValue -> JSValue
getFilmObject f@(JSObject filmInput) = case (valFromObj "film" filmInput) of
                                         (Error x) ->  f
                                         (Ok x) -> x

getOk :: Result x -> x
getOk (Ok x) = x
getOk (Error x) = trace x undefined

getDirectorBigBudgetFilms :: IO (Result [(String, Int)])
getDirectorBigBudgetFilms = getBigBudgetFilms "/film/director"

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
  let (i,_) = randomR (0,99) gen 
  return (directors !! i)

getDirectorFilmList :: IO (String,String,Result [String])
getDirectorFilmList = do
  director <- getDirector
  runSimpleQuery "/film/director" "film" director (JSArray []) (\(JSString x) -> fromJSString x)

getActorFilmList :: IO (String,String,Result [String])
getActorFilmList = do
  actor <- getActor
  let filmQueryObject = showJSON (toJSObject [("film", showJSON (toJSObject [("name", JSNull)]))])
  runSimpleQuery "/film/actor" "film" actor (JSArray [filmQueryObject]) extractFilmName
                      
extractFilmName :: JSValue -> String
extractFilmName (JSObject x) = fromJSString name
    where
      (Ok film) = valFromObj "film" x 
      (Ok (JSString name)) = valFromObj "name" film 

  
getActorBigBudgetFilms :: IO (Result [(String, Int)])
getActorBigBudgetFilms = do
  let budgetQueryObject = showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])
      filmQueryObject = showJSON (toJSObject [("film", showJSON (toJSObject [("name", JSNull), ("limit", showJSON (5 :: Int)), ("sort", showJSON "-estimated_budget.amount"), ("estimated_budget", budgetQueryObject)]))])
  response <- runQuery $ mkSimpleQuery [("type",showJSON "/film/actor"),("id",showJSON JSNull), ("limit",showJSON (110 :: Int)),("film", JSArray [filmQueryObject])]
  let arrayOfActorsAndFilms = (lookupValue response "result" :: Result JSValue)
  return (fmap extractIdAndBudgets arrayOfActorsAndFilms)
  
saveActorsToDisk :: IO ()
saveActorsToDisk = do
  (Ok films) <- getActorBigBudgetFilms
  writeFile actorPath (show $ map fst films)
        
readActorsFromDisk :: IO [String]
readActorsFromDisk = liftM read (readFile actorPath)

getActor :: IO String
getActor = do
  actors <- readActorsFromDisk
  gen <- newStdGen
  let (i,_) = randomR (0,99) gen 
  return (actors !! i)

