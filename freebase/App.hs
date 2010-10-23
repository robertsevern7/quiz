{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Films
import Logic

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
identifyFromTemplate description choices answer = [$hamlet|
  %h1 $description$
  %ul#c
    $forall choices c
      %li $c$  
    %input!type="text"!id="identifyFromAnswer"              
	%div!text=$answer$!id="identifyFromHiddenAnswer"
  |]

identifyTemplate :: Description -> [(String,String)] -> Hamlet (Route QuizMaster)
identifyTemplate description pairs = [$hamlet|
  %h1 $description$
  %input!type="text"!id="identifyAnswer"
  %ul#c
    $forall pairs p
      %li $snd p$
	  %li!class="hiddenAnswer" $fst p$
  |]
  
layout :: Cassius (Route QuizMaster)
layout = [$cassius|
  h1
    color: red
|]          

headTemplate :: Hamlet (Route QuizMaster)
headTemplate = [$hamlet|
  %title Quiz Master
  %link!rel="stylesheet"!href=@StaticR.albums_css@
  %link!rel="stylesheet"!href=@StaticR.app_css@
  %script!src="http://code.jquery.com/jquery-1.4.2.min.js"
  %script!src=@StaticR.script_js@
|]
                                             
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
getHomeR = hamletToRepHtml [$hamlet|
  !!!
  %html
    %head
      %title Quiz Master
      %link!rel="stylesheet"!href=@StaticR.albums_css@
	  %link!rel="stylesheet"!href=@StaticR.app_css@
      %script!src="http://code.jquery.com/jquery-1.4.2.min.js"
      %script!src=@StaticR.script_js@
    %body
      %h1 AwesomeQuiz.com
      %p AwesomeQuiz provides a wide ranging set of questions to tax your brain.  Pick from one of the following categories to get a randomly selected question.  Yes, it's crap at the moment, but that's because it's a beta.
      %ul
        %li
          %a!href=@DirectorsR@ Film Directors
        %li
          %a!href=@ActorsR@ Film Actors
		%li
          %a!href=@TaglinesR@ Film Taglines
      %hr
      %p Written using 
          %a!href="http://docs.yesodweb.com/Yesod" Yesod Web Framework
  |]              

main :: IO ()
main = do
  let static = fileLookupDir "static/" typeByExt
  wDirector <- mkWhichDirector
  wActor <- mkWhichActor
  wFilm <- mkWhichFilm
  basicHandler 3000 $ QuizMaster static wDirector wActor wFilm