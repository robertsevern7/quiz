module Logic (
  Question(Associate,Order,Identify,IdentifyText),
  QuestionType(AssociateType,OrderType,IdentifyType,IdentifyTextType,IdentifyMultipleType),
  QuestionMaker,
  generateQuestion,
  chooseFromList,
  rndSelect,
  shuffleIO
  ) where

import Yesod.Helpers.Static -- Too much coupling?
import System.Random (mkStdGen,randomR,StdGen)
import Data.Array.ST (readArray,writeArray,STArray,newListArray)
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Web.Routes.Quasi (SinglePiece,toSinglePiece,fromSinglePiece)
import qualified Data.Text as T

-- TODO This is awful?
-- The various types of questions that we have
data QuestionType = AssociateType 
                  | OrderType 
                  | IdentifyType 
                  | IdentifyTextType
                  | IdentifyMultipleType
                  deriving (Show,Read,Eq)
                           
instance SinglePiece QuestionType where
  toSinglePiece = T.pack . show
  fromSinglePiece x = Just (read (T.unpack x))
  
type Tolerance = Int

-- TODO some of these question types are less than self-explanatory
-- |All the different types of questions
data Question = 
  -- ^ Associates of LHS to RHS.  A given tolerance
  Associate Description [(String, String)] Tolerance 
  -- ^ An ordering of the first element, with supporting information in the second
  | Order Description [(String, String)] 
    -- ^ Identify some static resource as a string
  | Identify Description StaticRoute String
    -- ^ Straight question and answer with optional link for answer and a tolerance
  | IdentifyText Description String String (Maybe String) Tolerance
  deriving Show

-- |May want to change this to something "formattable"
type Description = String

-- |A question maker uses some logic to generate questions given the requests question type
-- |An integer is used to provide variation.
class QuestionMaker a where 
    generateQuestion :: Int -> QuestionType -> a -> IO (Maybe Question)
    
-- |Choose a random element from a list given a seed
chooseFromList :: Int -> [a] -> IO a
chooseFromList seed xs = do
  selection <- rndSelect seed xs 1
  return (head selection)

-- From http://www.haskell.org/haskellwiki/Random_shuffle
-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray x =  newListArray (1,x)  
    
shuffleIO :: Int -> [a] -> IO [a]
shuffleIO seed xs = return $ fst $ shuffle' xs (mkStdGen seed)

-- |Given a seed, select n items at random from the supplied list
rndSelect :: Int -> [a] -> Int -> IO [a]
rndSelect seed xs n 
  | n < 0     = error "N must be greater than zero."
  | otherwise = do
    shuffle' <- shuffleIO seed xs
    return $ take n shuffle'