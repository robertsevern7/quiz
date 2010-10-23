{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Films
import Logic

-- We can remove the debug suffix in production
import Text.Hamlet (hamletFileDebug)
import Text.Cassius (cassiusFileDebug)

{-
  TODO List
  * Templates
    http://docs.yesodweb.com/book/templates/
    Use Cassius for the stylesheet everywhere
    Load the files externally allowing work
    Use a default Layout (instance of Yesod)

  * Application
    Perhaps sim-hash can do finding out if the answer is right?

  * Quick wins
    Send the answer back in the HTML as a hidden thing, use JQuery
    to style things up based on the name of a div, client side scoring
-}

data QuizMaster = QuizMaster {
      ajaxStatic :: Static
    , whichDirector :: WhichDirector
    , whichActor :: WhichActor
    , whichFilm :: WhichFilm
}

type Handler = GHandler QuizMaster QuizMaster

staticFiles "static/"

mkYesod "QuizMaster" [$parseRoutes|                      
  /          HomeR   GET
  /static    StaticR Static ajaxStatic
  /actors    ActorsR GET
  /directors DirectorsR GET
  /taglines  TaglinesR GET
|]

instance Yesod QuizMaster where
    approot _ = ""

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
                                             
getQuestionSource :: QuestionMaker a => (QuizMaster -> a) -> Handler RepHtml
getQuestionSource getQuestion = do
  quizMaster <- getYesod
  question <- liftIO $ generateQuestion (getQuestion quizMaster)
  defaultLayout $ do
    addHead  headTemplate
    addBody  (questionTemplate question)
    addStyle layout

getActorsR :: Handler RepHtml
getActorsR = getQuestionSource whichActor

getDirectorsR :: Handler RepHtml
getDirectorsR = getQuestionSource whichDirector

getTaglinesR :: Handler RepHtml
getTaglinesR = getQuestionSource whichFilm
   
getHomeR :: Handler RepHtml
getHomeR = hamletToRepHtml $(hamletFileDebug "templates/homeTemplate.hamlet")

-- Note that you'll need to remember to ensure that the data files are present
-- by using the GenFilms package
main :: IO ()
main = do
  let static = fileLookupDir "static/" typeByExt
  wDirector <- mkWhichDirector
  wActor <- mkWhichActor
  wFilm <- mkWhichFilm
  basicHandler 3000 $ QuizMaster static wDirector wActor wFilm