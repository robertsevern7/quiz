module Logic where

import System.Random (mkStdGen,randomR)
import System.Random.MWC 
import Data.Random
import Data.Vector.Unboxed (singleton)

-- TODO some of these question types are less than self-explanatory
-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String -- ^ Choose one from a set
                    | FreeText String -- |^ Free text to compare against supplied text
                    | MultipleFreeText [String] -- |^ Multiple choices of free text 
                    | IdentifyFrom [String] String -- |^ Given a set of strings identify some known answer
                    | Identify [(String, String)] [String] -- |^ A list of answer/hint pairs
                      deriving Show

-- |May want to change this to something "formattable"
type Description = String

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat deriving Show

-- |A question maker uses some logic to generate questions
-- |An integer is used to provide variation
class QuestionMaker a where 
    generateQuestion :: Int -> a -> IO Question

-- |Choose a random element from a list given a seed
chooseFromList :: Int -> [String] -> String
chooseFromList seed xs = (xs !! i) 
  where
    g = mkStdGen seed
    len = length xs
    (i,_) = randomR (0,len) g
  
-- |Given a seed, select n items at random from the supplied list
rndSelect :: Int -> [a] -> Int -> IO [a]
rndSelect seed xs n 
  | n < 0     = error "N must be greater than zero."
  | otherwise = do
      let a = shuffle xs -- of type RVar [a]
      x <- initialize (singleton $ fromIntegral ((abs seed) `mod` (2 ^ 30)))
      shuffled <- runRVar a x
      return $ take n shuffled
      
-- TODO unnecessary since we can do it in the JS
shuffleHints :: Int -> [(String,  String)] -> IO [String]
shuffleHints seed input = do
  shuffled <- rndSelect seed input (length input)
  return $ map snd shuffled