module Anagram(
  ) where
  
import Logic (QuestionMaker,generateQuestion, rndSelect,Question(Question),QuestionFormat(Order))
import Data.Char
import List
  
--data Anagrams = Anagrams [(String,StaticRoute)]
  
--instance QuestionMaker Anagrams where
--  generateQuestion seed (Anagrams anagrams) = return $ Question "Unscramble this word?" (Identify scrambled word)
--    where
--      (scrambled, word) = chooseFromList seed anagrams
  
anagrams :: [String]
anagrams = [
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
  
sevenLetterWordsIn = "./AnagramData/7LetterInput.txt"
sevenLetterWordsOut = "./AnagramData/7LetterFiltered.txt"

grabRandom :: String
--grabRandom = rndSelect seed anagrams 1
grabRandom = head $ rndSelect 4 anagrams 1

shuffleWord :: [Char] -> [Char]
shuffleWord toShuffle = rndSelect 4 toShuffle (length toShuffle)

filterAnagramLists :: IO()
filterAnagramLists = nonAnagramList sevenLetterWordsIn

nonAnagramList :: String -> IO()
nonAnagramList file = do
  filecontent <- readFile file
  writeFile sevenLetterWordsOut (unlines (sort (filterWords (lines filecontent))))
  return ()

orderLetters :: [Char] -> [Char]
orderLetters = sort.(map toLower)

createKey :: String -> (String, String)
createKey input = (orderLetters input, input)

createMap :: [String] -> [(String, String)]
createMap = map createKey

groupWords :: [String] -> [[(String, String)]]
groupWords words = groupBy cond (sortKeys words)
  where cond :: (String,String) -> (String,String) -> Bool
        cond x y = (fst x) == (fst y)
        
sortKeys :: [String] -> [(String, String)]
sortKeys words = sortBy cmp (createMap words)
  where cmp :: (String,String) -> (String,String) -> Ordering
        cmp x y = compare (fst x) (fst y)
        
filterRepeats :: [(String,String)] -> Bool
filterRepeats input = (length input) == 1
        
retrieveWord :: [(String,String)] -> String
retrieveWord input = snd (head input)
        
filterWords :: [String] -> [String]
filterWords words = map retrieveWord (filter filterRepeats (groupWords words))