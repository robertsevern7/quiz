module Taglines where

import Freebase
import GenFilms
import Logic

import Data.Object
import Data.Object.Json
import qualified Data.ByteString.Char8 as B

import Control.Monad
import Data.Char (toLower)
import Data.Maybe (fromJust)

data TagLines = TagLines [String]

instance QuestionMaker TagLines where
    generateQuestion seed AssociateType (TagLines films) = do
      tagLines <- getTaglineFilmList (rndSelect seed films 10)
      let hidden = hideFilmNames tagLines
      return $ Just (Associate "Name the films from the taglines" hidden)

hideFilmNames :: [(String,String)] -> [(String,String)]
hideFilmNames = map redact 

redact :: (String,String) -> (String,String)
redact (film,tagline) = (film,unwords $ replacer (words film) (words tagline))

replacer :: [String] -> [String] -> [String]
replacer movieWords = map (redactionReturner movieWords) 

redactionReturner :: [String] -> String -> String
redactionReturner movieWords taglineWord | tagWord `elem` stopWords = taglineWord
                                         | tagWord `elem` mw = "_____"
                                         | otherwise = taglineWord 
                                           where
                                             mw = map lower movieWords
                                             tagWord = lower taglineWord

-- TODO This sucks
getTaglineFilmList :: [String] -> IO [(String,String)]
getTaglineFilmList filmIds = do
  results <- runQueryAndGetResult (tagLineQuery filmIds) 
  forM results (\x -> do
                   name <- fromScalar $ fromJust $ fromMapping x >>= lookup (B.pack "name")
                   tagline <- fromScalar $ snd $ head (head (head (fromSequence $ fromJust $ fromMapping x >>= lookup (B.pack "tagline")) >>= fromMapping)) 
                   return (fromJsonScalar name, fromJsonScalar tagline))

  
  --let arrayFilmsAndTags = lookupValue response "result"
  --return (fmap getTaglineFilmPairs arrayFilmsAndTags)
  
lower :: String -> String
lower = map toLower

stopWords :: [String]
stopWords = ["the","be","to","of","and","a","in","that","have","i","it","for","not","on","with","he","as","you","do","at","this","but","his","by","from","they","we","say","her","she","or","an","will","my","one","all","would","there","their","what","so","up","out","if","about","who","get","which","go","me","when","make","can","like","no","just","him","know","take","into","year","your","some","could","them","see","other","than","then","now","look","only","come","its","over","think","also","back","after","use","two","how","our","work","first","well","way","even","new","want","because","any","these","give","day","most","us"]