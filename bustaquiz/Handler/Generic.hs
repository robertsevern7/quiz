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

genericRoute :: QuestionMaker a => (Quiz -> a) -> (Int -> QuestionType -> QuizRoute) -> Int -> QuestionType -> String -> Handler RepHtml
genericRoute quizFunc nextFn seed questionType sloppinessFactor = do
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
  addWidget $(widgetFile "shuffle")
  addWidget $(widgetFile "hover")
  addWidget $(widgetFile "text")
  
  addWidget $(widgetFile "ladder")
  addWidget $(widgetFile "associate")
  addWidget $(widgetFile "buttons")

  
questionWidget (OrderType) route (Order description ordering) = do
  addWidget $(widgetFile "shuffle")

  addWidget $(widgetFile "ladder")
  addWidget $(widgetFile "ordering")
  addWidget $(widgetFile "buttons")
  addWidget $(widgetFile "hover")

questionWidget (IdentifyType) route (Identify description resource answer) = do
  addWidget $(widgetFile "text")
  
  addWidget $(widgetFile "ladder")  
  addWidget $(widgetFile "identify")
  addWidget $(widgetFile "buttons")

questionWidget (IdentifyTextType) route (IdentifyText description question answer link sloppinessFactorInt) = do
  let sloppinessFactor = show sloppinessFactorInt
  addWidget $(widgetFile "text")
  addWidget $(widgetFile "sloppy")
  addWidget $(widgetFile "ladder")
  addWidget $(widgetFile "identifyText")
  addWidget $(widgetFile "buttons")
  addJulius $(juliusFile "identify") -- TODO This can't be a widget because it breaks - WHY?
  
questionWidget (IdentifyMultipleType) route (Associate description pairs) = do
  addWidget $(widgetFile "text")
  
  addWidget $(widgetFile "ladder")
  addWidget $(widgetFile "identifyMultiple")
  addWidget $(widgetFile "buttons")
