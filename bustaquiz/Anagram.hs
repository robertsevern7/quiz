module Anagram(
  Anagrams,
  fiveLetterAnagrams,
  sixLetterAnagrams,
  sevenLetterAnagrams,
  eightLetterAnagrams
  ) where
  
import Logic (QuestionMaker,generateQuestion,rndSelect,Question(IdentifyText),QuestionType(IdentifyTextType))
import Data.Char
import Data.List
import Data.Ord (comparing)
import AnagramData.WordLists (fiveLetterList,sixLetterList,sevenLetterList,eightLetterList)
  
data Anagrams = Anagrams [String]

fiveLetterAnagrams :: Anagrams
fiveLetterAnagrams =  Anagrams fiveLetterList

sixLetterAnagrams :: Anagrams
sixLetterAnagrams =  Anagrams sixLetterList

sevenLetterAnagrams :: Anagrams
sevenLetterAnagrams =  Anagrams sevenLetterList

eightLetterAnagrams :: Anagrams
eightLetterAnagrams =  Anagrams eightLetterList

instance QuestionMaker Anagrams where
  generateQuestion seed IdentifyTextType (Anagrams wordList) = do
    wordL <- (rndSelect seed wordList 1) 
    let word = head wordL
    shuffled <- rndSelect seed word (length word)
    let link = Just ("http://www.google.com/search?q=define:" ++ word)
        desc = "Unscramble this word"
    return $ Just (IdentifyText desc shuffled word link)
  generateQuestion _ _ _ = return Nothing

allWords = "./AnagramData/allWords.txt"
popularWords = "./AnagramData/popularWords.txt"
popularWordsFiltered = "./AnagramData/popularWordsFiltered.txt"
  
sortOutRawInput :: IO()
sortOutRawInput = do
  popularFileContent <- readFile popularWords
  allFileContent <- readFile allWords
  writeFile popularWordsFiltered (unlines (sortBy (comparing length) (intersectLists (lines allFileContent) (cleanWords (lines popularFileContent)))))
    --where stringLength :: String -> String -> Ordering
    --      stringLength = comparing length

orderLetters :: String -> String
orderLetters = sort. map toLower

createKey :: String -> (String, String)
createKey input = (orderLetters input, input)

createMap :: [String] -> [(String, String)]
createMap = map createKey

groupWords :: [String] -> [[(String, String)]]
groupWords words = groupBy cond (sortKeys words)
  where cond :: (String,String) -> (String,String) -> Bool
        cond x y = fst x == fst y
        
sortKeys :: [String] -> [(String, String)]
sortKeys words = sortBy cmp (createMap words)
  where cmp :: (String,String) -> (String,String) -> Ordering
        cmp = comparing fst
        
filterRepeats :: [(String,String)] -> Bool
filterRepeats input = length input == 1
        
retrieveWord :: [(String,String)] -> String
retrieveWord input = snd (head input)
        
stripOutAnagrams :: [String] -> [String]
stripOutAnagrams words = map retrieveWord (filter filterRepeats (groupWords words))

intersectLists :: [String] -> [String] -> [String]
intersectLists fullList popularWords = filter containedBy (stripOutAnagrams fullList)
  where containedBy :: String -> Bool
        containedBy word = word `elem` popularWords

clean :: String -> Bool
clean input = length input > 4 && length input < 9 && map toLower input == input

cleanWords :: [String] -> [String]
cleanWords = filter clean