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

import System.Random
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
  /actors/#Int    ActorsR GET
  /directors/#Int DirectorsR GET
  /taglines/#Int  TaglinesR GET
  /capitals/#Int  CapitalsR GET
|]

instance Yesod QuizMaster where
    approot _ = "http://localhost:3000"

questionTemplate :: Question -> Hamlet (Route QuizMaster)
questionTemplate (Question description (IdentifyFrom choices answer)) =  $(hamletFileDebug "templates/identifyFromTemplate.hamlet") 
questionTemplate (Question description (Identify pairs)) = $(hamletFileDebug "templates/identifyTemplate.hamlet")
questionTemplate (Question _ _) = error "This has not been implemented yet."

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

runQuestion :: QuestionMaker a => Int -> a -> IO (Either QuizException Question)
runQuestion seed qm = try (evaluate =<< generateQuestion seed qm)
                                             
getQuestionSource :: QuestionMaker a => (QuizMaster -> a) -> Int -> Handler RepHtml
getQuestionSource getQuestion seed = do
  quizMaster <- getYesod
  generatedQuestion <- liftIO $ runQuestion seed (getQuestion quizMaster)
  case generatedQuestion of
    (Left ex)        -> invalidArgs ["Failed to generate valid question.", message ex, internal ex]
    (Right question) -> do
      let body = (questionTemplate question)
          questions = $(hamletFileDebug "templates/bodybase.hamlet")
      defaultLayout $ do
        addHamletHead  headTemplate
        addHamlet  questions
        addCassius layout

getActorsR :: Int -> Handler RepHtml
getActorsR seed = getQuestionSource whichActor seed

getDirectorsR :: Int -> Handler RepHtml
getDirectorsR seed = getQuestionSource whichDirector seed

getTaglinesR :: Int -> Handler RepHtml
getTaglinesR seed = getQuestionSource whichFilm seed

getCapitalsR :: Int -> Handler RepHtml
getCapitalsR seed = getQuestionSource whichCapital seed
   
homeTemplate :: Int -> Hamlet (Route QuizMaster)
homeTemplate ran = do
  let body = $(hamletFileDebug "templates/homeTemplate.hamlet")
  $(hamletFileDebug "templates/bodybase.hamlet")
   
getHomeR :: Handler RepHtml
getHomeR = 
  defaultLayout $ do
    gen <- liftIO $ newStdGen
    let ran = fst $ random gen
    addHamletHead headTemplate
    addHamlet (homeTemplate ran)

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
