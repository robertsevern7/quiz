{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Handler.Generic (
  runQuestion,
  questionWidget,
  genericRoute) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz 
import System.Random
import qualified Data.Text as T

runQuestion :: QuestionMaker a => Int -> QuestionType -> a -> IO (Either QuizException (Maybe Question))
runQuestion seed questionType qm = try (evaluate =<< generateQuestion seed questionType qm)
                                             
-- TODO - Should I try to set the title here?
genericRoute :: QuestionMaker a => (Quiz -> a) -> (Int -> QuestionType -> QuizRoute) -> Int -> QuestionType -> Handler RepHtml
genericRoute quizFunc nextFn seed questionType = do
  quiz <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed questionType (quizFunc quiz) 
  next <- liftIO $ getStdRandom (randomR (1,10000000))
  case generatedQuestion of
    (Left ex) -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right (Just question)) -> defaultLayout $ addWidget (questionWidget questionType (nextFn next questionType) question)
    -- TODO more information needed?
    -- TODO Push this into the type system?
    (Right Nothing) -> invalidArgs ["Failed to generate a question of the specified type", "Failed to generate question", T.pack $ show questionType]

-- Display the question as a widget
--questionWidget :: (Monad m, Route master ~ QuizRoute) => QuestionType -> t -> Question -> GGWidget sub master m ()
questionWidget (AssociateType) route (Associate description pairs) = do
  -- External requirements
  addJulius $(juliusFile "shuffle")
  addJulius $(juliusFile "hover")
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "ladder")
  addHamlet $(hamletFile "associate")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "associate")
  addJulius $(juliusFile "associate")
  addJulius $(juliusFile "ladder")
  
questionWidget (OrderType) route (Order description ordering) = do
  addJulius $(juliusFile "shuffle")

  addHamlet $(hamletFile "ladder")
  addHamlet $(hamletFile "ordering")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "ordering")
  addJulius $(juliusFile "ordering")
  addJulius $(juliusFile "hover")
  addJulius $(juliusFile "ladder")

questionWidget (IdentifyType) route (Identify description resource answer) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "ladder")  
  addHamlet $(hamletFile "identify")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "identify")
  addJulius $(juliusFile "identify")
  addJulius $(juliusFile "ladder")

questionWidget (IdentifyTextType) route (IdentifyText description question answer link) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "ladder")
  addHamlet $(hamletFile "identifyText")
  addHamlet $(hamletFile "buttons")
  addJulius $(juliusFile "identify")
  addJulius $(juliusFile "ladder")
  
questionWidget (IdentifyMultipleType) route (Associate description pairs) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "ladder")
  addHamlet $(hamletFile "identifyMultiple")
  addHamlet $(hamletFile "buttons")
  addJulius $(juliusFile "identifyMultiple")
  addJulius $(juliusFile "ladder")