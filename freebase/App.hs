{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, DeriveDataTypeable #-}

import Films
import Country (CapitalQuiz,capitalQuiz)
import Logic
import Exception
import GenFilms

import Yesod
import Yesod.Helpers.Static
import Text.Hamlet (hamletFileDebug,hamletFile) -- We can remove the debug suffix in production
import Text.Cassius (cassiusFileDebug,cassiusFile)
import Control.Exception (try,evaluate)
-- import Network.Wai.Handler.FastCGI (run)

import System.Console.CmdArgs

{-
  TODO List
  * Application
    Perhaps sim-hash can do finding out if the answer is right?

  * Quick wins
    Send the answer back in the HTML as a hidden thing, use JQuery
    to style things up based on the name of a div, client side scoring
-}

-- Command line parameters
data Config = Config {
  -- |If true, run using simple localhost server on port 3000
  local :: Bool,
  -- |If true, run using debug templates,
  debug :: Bool,
  -- |If true, regenerate the film data
  generateBaseData :: Bool
} deriving (Show,Data,Typeable)

config :: Config
config = Config{
  local = def &= help "If true, runs on local host",
  debug = def &= help "If true, then debug file handlers are used",
  generateBaseData = def &= help "If true, regenerates the base data"
}

data QuizMaster = QuizMaster {
      ajaxStatic :: Static
    , whichDirector :: WhichDirector
    , whichActor :: WhichActor
    , whichFilm :: WhichFilm
    , whichCapital :: CapitalQuiz
}

type Handler = GHandler QuizMaster QuizMaster

staticFiles "static/"

mkYesod "QuizMaster" [$parseRoutes|                      
  /          HomeR   GET
  /static    StaticR Static ajaxStatic
  /actors    ActorsR GET
  /directors DirectorsR GET
  /taglines  TaglinesR GET
  /capitals  CapitalsR GET
|]

instance Yesod QuizMaster where
    approot _ = "http://localhost:3000"

questionTemplate :: Question -> Hamlet (Route QuizMaster)
questionTemplate (Question description (IdentifyFrom choices answer)) = identifyFromTemplate description choices answer
questionTemplate (Question description (Identify pairs)) = identifyTemplate description pairs 
questionTemplate (Question _ _) = error "This has not been implemented yet."

identifyFromTemplate :: Description -> [String] -> String -> Hamlet (Route QuizMaster)
identifyFromTemplate description choices answer = $(hamletFileDebug "templates/identifyFromTemplate.hamlet")

identifyTemplate :: Description -> [(String,String)] -> Hamlet (Route QuizMaster)
identifyTemplate description pairs = $(hamletFileDebug "templates/identifyTemplate.hamlet")

layout :: Cassius (Route QuizMaster)
layout = $(cassiusFileDebug "templates/style.cassius")

headTemplate :: Hamlet (Route QuizMaster)
headTemplate = $(hamletFileDebug "templates/headTemplate.hamlet")

topbarTemplate :: Hamlet (Route QuizMaster)
topbarTemplate = $(hamletFileDebug "templates/topbarTemplate.hamlet")

bottombarTemplate :: Hamlet (Route QuizMaster)
bottombarTemplate = $(hamletFileDebug "templates/bottombarTemplate.hamlet")

answerControlsTemplate :: Hamlet (Route QuizMaster)
answerControlsTemplate = $(hamletFileDebug "templates/footTemplate.hamlet")

runQuestion :: QuestionMaker a => a -> IO (Either QuizException Question)
runQuestion qm = try (evaluate =<< generateQuestion qm)
                                             
getQuestionSource :: QuestionMaker a => (QuizMaster -> a) -> Handler RepHtml
getQuestionSource getQuestion = do
  quizMaster <- getYesod
  generatedQuestion <- liftIO $ runQuestion (getQuestion quizMaster)
  case generatedQuestion of
    (Left ex)        -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right question) -> do
      let body = (questionTemplate question)
          questions = $(hamletFileDebug "templates/bodybase.hamlet")
      defaultLayout $ do
        addHamletHead  headTemplate
        addHamlet  questions
        addCassius layout

getActorsR :: Handler RepHtml
getActorsR = getQuestionSource whichActor

getDirectorsR :: Handler RepHtml
getDirectorsR = getQuestionSource whichDirector

getTaglinesR :: Handler RepHtml
getTaglinesR = getQuestionSource whichFilm

getCapitalsR :: Handler RepHtml
getCapitalsR = getQuestionSource whichCapital
   
homeTemplate :: Hamlet (Route QuizMaster)
homeTemplate = do
  let body = $(hamletFileDebug "templates/homeTemplate.hamlet")
  $(hamletFileDebug "templates/bodybase.hamlet")
   
getHomeR :: Handler RepHtml
getHomeR = 
  defaultLayout $ do
    addHamletHead headTemplate
    addHamlet homeTemplate

-- Note that you'll need to remember to ensure that the data files are present
-- by using the GenFilms package
startService :: Config -> IO ()    
startService runConfig = do
  let static = fileLookupDir "static/" typeByExt
  wDirector <- mkWhichDirector
  wActor <- mkWhichActor
  wFilm <- mkWhichFilm
  let quiz = QuizMaster static wDirector wActor wFilm capitalQuiz
  if (local runConfig)
    then basicHandler 3000 quiz
    else undefined -- toWaiApp quiz >>= run
    
main :: IO ()
main = startService =<< cmdArgs config
