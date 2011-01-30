module Taglines (
  FilmTaglines,
  filmTaglines,
  getAllFilmsWithTaglines
  ) where

import Freebase
import GenFilms
import Logic

import Data.Object
import Data.List
import Data.Object.Json
import qualified Data.ByteString.Char8 as B

import Control.Monad
import Data.Char (toLower)
import Data.Maybe (fromJust)

data FilmTaglines = FilmTaglines [String]

editedFilmList = "data/editedFilmList.txt"
filmList = "data/filmList.txt"

-- TODO replace with a big list of decent films
filmTaglines :: IO FilmTaglines
filmTaglines = do
  films <- readFile editedFilmList
  return (FilmTaglines (read films))

-- TDOO see CapitalQuiz, duplication 
instance QuestionMaker FilmTaglines where
    generateQuestion seed AssociateType (FilmTaglines films) = do
      selectedFilms <- rndSelect seed films 10
      tagLines <- getTaglineFilmList selectedFilms
      let hidden = hideFilmNames tagLines
      return $ Just (Associate "Match the films with their taglines" hidden)        
    generateQuestion seed IdentifyMultipleType (FilmTaglines films) = do
      selectedFilms <- rndSelect seed films 10
      tagLines <- getTaglineFilmList selectedFilms    
      let hidden = hideFilmNames tagLines
      return $ Just (Associate "Name the films from the taglines" hidden)
    generateQuestion _ _ _ = return Nothing

hideFilmNames :: [(String,String)] -> [(String,String)]
hideFilmNames = map redact 

redact :: (String,String) -> (String,String)
redact (tagline,film) = (unwords $ replacer (words film) (words tagline),film)

replacer :: [String] -> [String] -> [String]
replacer movieWords = map (redactionReturner movieWords) 

redactionReturner :: [String] -> String -> String
redactionReturner movieWords taglineWord | tagWord `elem` stopWords = taglineWord
                                         | tagWord `elem` mw = "_____"
                                         | otherwise = taglineWord 
                                           where
                                             mw = map lower movieWords
                                             tagWord = lower taglineWord

getAllFilmsWithTaglines :: IO [String]
getAllFilmsWithTaglines = do
  results <- runQueryAndGetResult filmsWithTaglines
  forM results (\x -> do
                   filmId <- fromScalar $ fromJust $ fromMapping x >>= lookup (B.pack "id")
                   return $ fromJsonScalar filmId)
                  
getTopFilms :: IO [String]
getTopFilms = do
  results <- runQueryAndGetResult topFilms
  forM results (\x -> do
                   item <- fromSequence $ fromJust $ fromMapping x >>= lookup (B.pack "item")
                   id <- fromScalar $ fromJust $ fromMapping (head item) >>= lookup (B.pack "id")
                   return $ fromJsonScalar id)
                   
saveTopFilmListToDisk :: IO()
saveTopFilmListToDisk = do
    taglineFilms <- getAllFilmsWithTaglines
    topFilms <- getTopFilms
    let list = taglineFilms `intersect` topFilms
    writeFile filmList (show list)
    
saveFilmListToDisk :: [String] -> IO ()
saveFilmListToDisk x = writeFile filmList (show x)

-- TODO This sucks
getTaglineFilmList :: [String] -> IO [(String,String)]
getTaglineFilmList filmIds = do
  results <- runQueryAndGetResult (tagLineQuery filmIds) 
  forM results (\x -> do
                   name <- fromScalar $ fromJust $ fromMapping x >>= lookup (B.pack "name")
                   tagline <- fromScalar $ snd $ head (head (head (fromSequence $ fromJust $ fromMapping x >>= lookup (B.pack "tagline")) >>= fromMapping)) 
                   return (fromJsonScalar tagline, fromJsonScalar name))

  
  --let arrayFilmsAndTags = lookupValue response "result"
  --return (fmap getTaglineFilmPairs arrayFilmsAndTags)
  
lower :: String -> String
lower = map toLower

stopWords :: [String]
stopWords = ["the","be","to","of","and","a","in","that","have","i","it","for","not","on","with","he","as","you","do","at","this","but","his","by","from","they","we","say","her","she","or","an","will","my","one","all","would","there","their","what","so","up","out","if","about","who","get","which","go","me","when","make","can","like","no","just","him","know","take","into","year","your","some","could","them","see","other","than","then","now","look","only","come","its","over","think","also","back","after","use","two","how","our","work","first","well","way","even","new","want","because","any","these","give","day","most","us"]