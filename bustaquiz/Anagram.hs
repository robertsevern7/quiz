module Anagram(
  Anagrams,
  anagrams
  ) where
  
import Logic (QuestionMaker,generateQuestion, rndSelect,Question(Question),QuestionFormat(IdentifyText))
import Data.Char
import Data.List
import Data.Ord (comparing)
  
data Anagrams = Anagrams [String]

anagrams :: Anagrams
anagrams =  Anagrams wordList

instance QuestionMaker Anagrams where
  generateQuestion seed (Anagrams wordList) = return $ Question desc (IdentifyText shuffled word)
    where 
      word = head $ rndSelect seed wordList 1
      shuffled = rndSelect seed word (length word)
      desc = "Unscramble this word"

wordList :: [String]
wordList = [
  "abusing",
  "abusive",
  "abuttal",
  "abutted",
  "abutter",
  "abvolts",
  "abwatts",
  "abysmal",
  "abyssal"
  ]

fiveLetterWordsIn = "./AnagramData/5LetterInput.txt"
fiveLetterWordsOut = "./AnagramData/5LetterFiltered.txt"
sixLetterWordsIn = "./AnagramData/6LetterInput.txt"
sixLetterWordsOut = "./AnagramData/6LetterFiltered.txt"
sevenLetterWordsIn = "./AnagramData/7LetterInput.txt"
sevenLetterWordsOut = "./AnagramData/7LetterFiltered.txt"
eightLetterWordsIn = "./AnagramData/8LetterInput.txt"
eightLetterWordsOut = "./AnagramData/8LetterFiltered.txt"

filterAnagramLists :: IO()
filterAnagramLists = do
  nonAnagramList fiveLetterWordsIn fiveLetterWordsOut
  nonAnagramList sixLetterWordsIn sixLetterWordsOut
  nonAnagramList sevenLetterWordsIn sevenLetterWordsOut
  nonAnagramList eightLetterWordsIn eightLetterWordsOut
  
nonAnagramList :: String -> String -> IO()
nonAnagramList fileIn fileOut = do
  filecontent <- readFile fileIn
  writeFile fileOut (unlines (sort (filterWords (lines filecontent))))
  return ()

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
        
filterWords :: [String] -> [String]
filterWords words = map retrieveWord (filter filterRepeats (groupWords words))