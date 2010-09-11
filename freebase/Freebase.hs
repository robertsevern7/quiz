module Freebase where

import System.Random
import Text.JSON
import Text.JSON.Types
import Network.HTTP
import Network.URI
import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Monad

import Data.Maybe (fromJust)

getFirst :: Result JSValue -> Result JSValue
getFirst (Ok (JSArray (x:xs))) = Ok x
getFirst _ = Error "Not an array"

-- Should this use fmap?
lookupValue :: JSON a => Result JSValue -> String -> Result a
lookupValue (Ok (JSObject o)) key = valFromObj key o
lookupValue _ _                   = Error "Unsupported JSON response" 

touch :: URI
touch = fromJust $ parseURI "http://api.freebase.com/api/service/touch" 

status :: URI
status = fromJust $ parseURI "http://api.freebase.com/api/status" 

version :: URI
version = fromJust $ parseURI "http://api.freebase.com/api/version" 

simpleService :: URI -> IO (Result JSValue)
simpleService s = liftM decode (simpleHTTP (mkRequest GET s) >>= getResponseBody)

mqlReadUri :: String
mqlReadUri = "http://api.freebase.com/api/service/mqlread"

makeQuery :: JSValue -> IO (Result JSValue)
makeQuery s = liftM decode (simpleHTTP (getRequest (mqlReadUri ++ "?query=" ++ urlEncode (encode s))) >>= getResponseBody) 

mkSimpleQuery :: [(String,JSValue)] -> JSValue
mkSimpleQuery x = JSObject $ toJSObject [("query", JSArray [JSObject $ toJSObject x])]

runSimpleQuery :: String -> String -> String -> IO (Result [String])
runSimpleQuery qtype key name = do
  response <- makeQuery $ mkSimpleQuery [("type",showJSON qtype),("name",showJSON name),(key, JSArray [])]
  let k = lookupValue response "result"
      l = lookupValue (getFirst k) key
      m = fmap (\(JSArray x) -> x) l
  return $ fmap (map (\(JSString x) -> fromJSString x)) m
  

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
getAlbumList :: String -> IO (Result [String])
getAlbumList = runSimpleQuery "/music/artist" "album" 

getFilmList :: String -> IO (Result [String])
getFilmList = runSimpleQuery "/film/director" "film"

getRandomFilmList :: IO (Result [String])
getRandomFilmList = do
  gen <- newStdGen
  let len = length listOfDirectors
      (a,_) = randomR (0,len-1) gen
  getFilmList (listOfDirectors !! a)


-- don't hardcode, but easy for now!
listOfDirectors :: [String]
listOfDirectors = ["Blake Edwards",
                   "D. A. Pennebaker",
                   "Chris Hegedus",
                   "David Dawkins",
                   "Zack Snyder",
                   "Bernardo Bertolucci",
                   "Steven Spielberg",
                   "Michael Radford",
                   "Wong Kar-wai",
                   "John Landis",
                   "Luis Mandoki",
                   "Norman Jewison",
                   "Pedro Almodóvar",
                   "Francis Delia",
                   "Scott Norlund",
                   "Mark Osborne",
                   "Dror Soref",
                   "Robert K. Weiss",
                   "Hal Warren",
                   "Michael Dimich",
                   "Bharathiraja",
                   "Aparna Sen",
                   "Nathan Juran",
                   "George Seaton",
                   "John Singleton",
                   "Phil Karlson",
                   "Jean-Jacques Beineix",
                   "Joel Schumacher",
                   "Henry Hathaway",
                   "Richard Fleischer",
                   "Selvaraghavan",
                   "Kim Ki-duk",
                   "Delmer Daves"]