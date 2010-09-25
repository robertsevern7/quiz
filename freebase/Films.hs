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

import System.Random
import Text.JSON
import Text.JSON.Types
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Control.Monad

data WhichDirector = WhichDirector [String]
data WhichActor = WhichActor [String]

-- TODO These guys haven't been tested since the great reshuffle.
data FilmListDirectorQM = FilmListDirectorQM [String]
data FilmListActorQM = FilmListActorQM

mkWhichDirector :: IO WhichDirector
mkWhichDirector = liftM WhichDirector (readDirectorsFromDisk directorPath)

instance QuestionMaker WhichDirector where
    generateQuestion (WhichDirector directors) = do
                                       films <- getDirectorFilmList =<< (chooseFromList directors)
                                       return $ generateQuestionWhoMadeThese "Who directed the following films?" films

instance QuestionMaker FilmListDirectorQM where
    generateQuestion _ = do
        films <- getDirectorFilmList directorPath
        return $ generateQuestionNameTheFilm films
		
mkWhichActor :: IO WhichActor
mkWhichActor = liftM WhichActor (readActorsFromDisk actorPath)

instance QuestionMaker WhichActor where
	generateQuestion (WhichActor films) = do
                                    actors <- getActorFilmList =<< (chooseFromList films)
                                    return $ generateQuestionWhoMadeThese "Who acted in the following films?" actors
		
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
getFilmBudget f@(JSObject _) = truncate cost
    where
      (JSObject filmObject) = getFilmObject f
      (JSObject estimatedBudget) = fromJust $ get_field filmObject "estimated_budget"
      (JSRational _ cost) = fromJust $ get_field estimatedBudget "amount"

getFilmObject :: JSValue -> JSValue
getFilmObject f@(JSObject filmInput) = case (valFromObj "film" filmInput) of
                                         (Error _) ->  f
                                         (Ok x) -> x

getString :: Result JSValue -> String
getString (Ok (JSString x)) = fromJSString x
getString x = error $ "No string found when expected. =" ++ show x

getDirectorBigBudgetFilms :: IO (Result [(String, Int)])
getDirectorBigBudgetFilms = getBigBudgetFilms "/film/director"

saveDirectorsToDisk :: IO ()
saveDirectorsToDisk = do
  (Ok films) <- getDirectorBigBudgetFilms
  writeFile directorPath (show $ map fst films)
        
readDirectorsFromDisk :: String -> IO [String]
readDirectorsFromDisk path = liftM read (readFile path)


getDirector :: String -> IO String
getDirector path = do
  directors <- readDirectorsFromDisk path
  gen <- newStdGen
  let (i,_) = randomR (0,99) gen 
  return (directors !! i)

getDirectorFilmList :: String -> IO (String,Result [String])
getDirectorFilmList director = runSimpleQuery "/film/director" "film" director (JSArray [filmQueryObject]) extractFilmName2
	where
      filmQueryObject = showJSON (toJSObject [("estimated_budget", showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])), ("name", JSNull), ("limit", listLimit), ("sort", showJSON "-estimated_budget.amount")])
	  
extractFilmName2 :: JSValue -> String
extractFilmName2 (JSObject x) = fromJSString film
    where
      (Ok film) = valFromObj "name" x 
	  
getActorFilmList :: String -> IO (String,Result [String])
getActorFilmList actor = runSimpleQuery "/film/actor" "film" actor (JSArray [filmQueryObject]) extractFilmName
    where
		filmQueryObject = showJSON $ toJSObject [("film", filmObj), ("limit", listLimit), ("sort", showJSON "-film.estimated_budget.amount")]
		filmObj = showJSON (toJSObject [("estimated_budget", showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])), ("name", JSNull)])
                      
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
  
saveActorsToDisk :: String -> IO ()
saveActorsToDisk path = do
  (Ok films) <- getActorBigBudgetFilms
  writeFile path (show $ map fst films)
        
readActorsFromDisk :: String -> IO [String]
readActorsFromDisk path = liftM read (readFile path)

getActor :: String -> IO String
getActor path = do
  actors <- readActorsFromDisk path
  gen <- newStdGen
  let (i,_) = randomR (0,20) gen 
  return (actors !! i)

