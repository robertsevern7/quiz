module Logic (
  Question(Associate,Order,Identify,IdentifyText),
  QuestionType(AssociateType,OrderType,IdentifyType,IdentifyTextType),
  QuestionMaker,
  generateQuestion,
  chooseFromList,
  rndSelect
  ) where

import Yesod.Helpers.Static -- Too much coupling?
import System.Random (mkStdGen,random)

import Web.Routes.Quasi (SinglePiece,toSinglePiece,fromSinglePiece)

-- TODO This is awful?
-- The various types of questions that we have
data QuestionType = AssociateType 
                  | OrderType 
                  | IdentifyType 
                  | IdentifyTextType
                  deriving (Show,Read,Eq)
                           
instance SinglePiece QuestionType where
  toSinglePiece x = show x
  -- TODO errr, error handling and important stuff like that?  return (Left x) on error
  fromSinglePiece x = Right (read x)
                                                               

-- TODO some of these question types are less than self-explanatory
-- |All the different types of questions
data Question = Associate Description [(String, String)] -- ^ Associates of LHS to RHS
              | Order Description [(String, String)] -- ^ An ordering of the first element, with supporting information in the second
              | Identify Description StaticRoute String -- ^ Identify some static resource as a string
              | IdentifyText Description String String (Maybe String)-- ^ Straight question and answer with optional link for answer
              deriving Show

-- |May want to change this to something "formattable"
type Description = String

-- |A question maker uses some logic to generate questions given the requests question type
-- |An integer is used to provide variation
class QuestionMaker a where 
    generateQuestion :: Int -> QuestionType -> a -> IO (Maybe Question)

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

perm :: [a] -> Int -> [a]
perm xs k = [xs !! i | i <- dfr (radixRepresentation (length xs) k)]