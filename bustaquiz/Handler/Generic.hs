{-# LANGUAGE QuasiQuotes, OverloadedStrings, TemplateHaskell #-}
module Handler.Generic (
  runQuestion,
  questionWidget,
  genericRoute) where

import Control.Exception (try,evaluate)
import Logic
import Exception
import Quiz 
import Settings (cassiusFile,hamletFile,juliusFile,jqueryURL,jqueryUIURL)

runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
-- TODO - Should I try to set the title here?
genericRoute :: QuestionMaker a => Int -> (Quiz -> a) -> QuizRoute -> Handler RepHtml
genericRoute seed quizFunc next = do
  quiz <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed (quizFunc quiz) 
  case generatedQuestion of
    (Left ex) -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right question) -> defaultLayout $ do
      addWidget (questionWidget next (question :: Question))

-- TODO Bad things happen if I put a type signature in place here.
-- TODO Not entirely sure why I can't get rid of the repetition here      
-- TODO Something to do with Template Haskell
-- Display the question as a widget
questionWidget route (Question description (Associate pairs)) = do
  -- External requirements
  addJulius $(juliusFile "shuffle")
  addJulius $(juliusFile "text")
  
  -- Actual code
  addHamlet $(hamletFile "associate")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "associate")
  addJulius $(juliusFile "associate")
  
questionWidget route (Question description (Order ordering)) = do
  addJulius $(juliusFile "shuffle")

  addHamlet $(hamletFile "ordering")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "ordering")
  addJulius $(juliusFile "ordering")

questionWidget route (Question description (Identify resource answer)) = do
  addJulius $(juliusFile "text")
  
  addHamlet $(hamletFile "identify")
  addHamlet $(hamletFile "buttons")
  addCassius $(cassiusFile "identify")
  addJulius $(juliusFile "identify")
