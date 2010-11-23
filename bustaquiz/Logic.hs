module Logic (
  QuestionFormat(Associate,Order,Identify),
  Question(Question),
  QuestionMaker,
  description,
  generateQuestion,
  chooseFromList,
  rndSelect
  ) where

import StaticFiles
import Yesod.Helpers.Static -- Too much coupling?
import System.Random (mkStdGen,random,randomR)

-- TODO some of these question types are less than self-explanatory
-- |All the different types of questions
data QuestionFormat = Associate [(String, String)] -- ^ Associates of LHS to RHS
                    | Order [(String, String)] -- ^ An ordering of the first element, with supporting information in the second
                    | Identify StaticRoute String -- ^ Identify some static resource as a string
                      deriving Show

-- |May want to change this to something "formattable"
type Description = String

-- |A question is a question format, together with a description
data Question = Question Description QuestionFormat deriving Show

description :: Question -> Description
description (Question d _) = d

-- |A question maker uses some logic to generate questions
-- |An integer is used to provide variation
class QuestionMaker a where 
    generateQuestion :: Int -> a -> IO Question

-- |Choose a random element from a list given a seed
chooseFromList :: Int -> [a] -> a
chooseFromList seed xs = head $ rndSelect seed xs 1
  
-- |Given a seed, select n items at random from the supplied list
rndSelect :: Int -> [a] -> Int -> [a]
rndSelect seed xs n 
  | n < 0     = error "N must be greater than zero."
  | otherwise = take n (perm xs r)
    where
      g = mkStdGen seed
      (r,_) = random g
    
-- The following is based on http://en.literateprograms.org/Kth_permutation_(Haskell)
-- which comes from IVerson's approac
radixRepresentation :: Int -> Int -> [Int]
radixRepresentation 0 _ = []
radixRepresentation n k = k `mod` n : radixRepresentation (n-1) (k `div` n)

dfr :: [Int] -> [Int]
dfr = foldr (\x rs -> x : [r + (if x <= r then 1 else 0) | r <- rs]) []

par :: [Int] -> Int
par rs = sum rs `mod` 2

perm :: [a] -> Int -> [a]
perm xs k = [xs !! i | i <- dfr (radixRepresentation (length xs) k)]