{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion,
  questionWidget,
  genericRoute) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz 

runQuestion :: QuestionMaker a => Int -> QuestionType -> a -> IO (Either QuizException (Maybe Question))
runQuestion seed questionType qm = try (evaluate =<< generateQuestion seed questionType qm)
                                             
-- TODO - Should I try to set the title here?
genericRoute :: QuestionMaker a => Int -> QuestionType -> (Quiz -> a) -> QuizRoute -> Handler RepHtml
genericRoute seed questionType quizFunc next = do
  quiz <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed questionType (quizFunc quiz) 
  case generatedQuestion of
    (Left ex) -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right (Just question)) -> defaultLayout $ addWidget (questionWidget questionType next question)
    -- TODO more information needed?
    -- TODO Push this into the type system?
    (Right Nothing) -> invalidArgs ["Failed to generate a question of the specified type", "Failed to generate question", show questionType]

-- TODO Bad things happen if I put a type signature in place here.
-- TODO Not entirely sure why I can't get rid of the repetition here      
-- TODO Something to do with Template Haskell
-- Display the question as a widget
--TODO this needs to switch on QuestionType
questionWidget (AssociateType) route (Associate description pairs) = do
  -- External requirements
  addJulius $(juliusFile "shuffle")
  addJulius $(juliusFile "text")
  
  -- Actual code
  addHamlet $(hamletFile "associate")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "associate")
  addJulius $(juliusFile "associate")
  
questionWidget (OrderType) route (Order description ordering) = do
  addJulius $(juliusFile "shuffle")

  addHamlet $(hamletFile "ordering")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "ordering")
  addJulius $(juliusFile "ordering")

questionWidget (IdentifyType) route (Identify description resource answer) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "identify")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "identify")
  addJulius $(juliusFile "identify")

questionWidget (IdentifyTextType) route (IdentifyText description question answer link) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "identifyText")
  addHamlet $(hamletFile "buttons")
  addJulius $(juliusFile "identify")
  
questionWidget (IdentifyMultipleType) route (Associate description pairs) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "identifyMultiple")
  addHamlet $(hamletFile "buttons")
  addJulius $(juliusFile "identifyMultiple")