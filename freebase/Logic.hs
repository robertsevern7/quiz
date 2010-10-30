module Logic where

import System.Random (newStdGen,randomR)
import System.Random.MWC 
import Data.Random

-- TODO some of these question types are less than self-explanatory
-- |All the different types of questions
data QuestionFormat = MultipleChoice [String] String -- ^ Choose one from a set
                    | FreeText String -- |^ Free text to compare against supplied text
                    | MultipleFreeText [String] -- |^ Multiple choices of free text 
                    | IdentifyFrom [String] String -- |^ Given a set of strings identify some known answer
                    | Identify [(String, String)] -- |^ A list of answer/hint pairs
                      deriving Show

-- |May want to change this to something "formattable"
type Description = String

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat deriving Show

-- |A question maker uses some logic to generate questions
-- |An integer is used to provide variation
class QuestionMaker a where 
    generateQuestion :: Integer -> a -> IO Question

-- |Choose a random element from a list
chooseFromList :: [String] -> IO String
chooseFromList xs = do
  g <- newStdGen
  let len = length xs
      (i,_) = randomR (0,len) g
  return (xs !! i)
  
rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n 
  | n < 0     = error "N must be greater than zero."
  | otherwise = do
      let a = shuffle xs -- of type RVar [a]
      x <- create -- TODO broken
      shuffled <- runRVar a x
      return $ take n shuffled
      
