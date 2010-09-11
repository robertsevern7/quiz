module Films where

import System.Random
import Text.JSON
import Text.JSON.Types
import Network.HTTP
import Network.URI
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad
import Freebase

import Data.Maybe (fromJust)
directorPath :: FilePath
directorPath = "film/film_directors.txt"

actorPath :: FilePath
actorPath = "film/film_actors.txt"

getDirectorBigBudgetFilms :: IO (Result [(String, Int)])
getDirectorBigBudgetFilms = do
  let budgetQueryObject = showJSON (toJSObject [("amount", JSNull), ("currency", showJSON "US$")])
      filmQueryObject = showJSON (toJSObject [("name", JSNull), ("limit", showJSON (5 :: Int)), ("sort", showJSON "-estimated_budget.amount"), ("estimated_budget", budgetQueryObject)]);
  response <- makeQuery $ mkSimpleQuery [("type",showJSON "/film/director"),("id",showJSON JSNull), ("limit",showJSON (600 :: Int)),("film", JSArray [filmQueryObject])]
  let arrayOfDirAndFilms = (lookupValue response "result" :: Result JSValue)
  return (fmap extractDirAndBudgets arrayOfDirAndFilms)
  
extractDirAndBudgets :: JSValue -> [(String,Int)]
extractDirAndBudgets (JSArray xs) = sortBy (\(_,a) (_,b) -> compare b a) $ map extractDirAndBudget xs
extractDirAndBudgets _ = error "Freebase screwed us."
  
extractDirAndBudget :: JSValue -> (String,Int)
extractDirAndBudget (JSObject s) = (idDir, extractBudget $ fromJust $ get_field s "film")
	where
		idDir = (\(JSString z) -> fromJSString z) (fromJust $ get_field s "id")
extractDirAndBudget _ = undefined

extractBudget :: JSValue -> Int
extractBudget (JSArray films) = sum $ map getFilmBudget films

getFilmBudget :: JSValue -> Int
getFilmBudget (JSObject film) = truncate cost 
	where
		(JSObject estimatedBudget) = fromJust $ get_field film "estimated_budget"
		(JSRational _ cost) = fromJust $ get_field estimatedBudget "amount"

{-
	Directors
-}		
saveDirectorsToDisk :: IO ()
saveDirectorsToDisk = do
	(Ok films) <- getDirectorBigBudgetFilms
	writeFile directorPath (show $ map fst films)
	
readDirectorsFromDisk :: IO [String]
readDirectorsFromDisk = liftM read (readFile directorPath)

getDirector :: IO String
getDirector = do
	films <- readDirectorsFromDisk
	gen <- newStdGen
	let (i,_) = randomR (0,99) gen 
	return (films !! i)

getDirectorFilmList :: IO (String,Result [String])
getDirectorFilmList = do
  director <- getDirector
  runSimpleQuery "/film/director" "film" director