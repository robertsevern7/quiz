{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Films
import Logic

data QuizMaster = QuizMaster {
      ajaxStatic :: Static
    , whichDirector :: WhichDirector
    , whichActor :: WhichActor
}

type Handler = GHandler QuizMaster QuizMaster

staticFiles "static/"

mkYesod "QuizMaster" [$parseRoutes|                      
  /          HomeR   GET
  /static    StaticR Static ajaxStatic
  /actors    ActorsR GET
  /directors DirectorsR GET
|]

instance Yesod QuizMaster where
    approot _ = ""

questionTemplate :: Question -> Hamlet (Route QuizMaster)
questionTemplate (Question description (IdentifyFrom choices answer)) = identifyFromTemplate description choices answer
questionTemplate (Question description _) = error "This has not been implemented yet."

identifyFromTemplate :: Description -> [String] -> String -> Hamlet (Route QuizMaster)
identifyFromTemplate description choices answer = undefined [$hamlet|
  %html
    %head
      %title $description$
    %body
      %h1 $description$
      %p And some rendering of said question here.
  |]
                                                   
getActorsR :: Handler RepHtml
getActorsR = undefined

getDirectorsR :: Handler RepHtml
getDirectorsR = undefined
   
getHomeR :: Handler RepHtml
getHomeR = hamletToRepHtml [$hamlet|
  !!!
  %html
    %head
      %title Quiz Master
      %link!rel="stylesheet"!href=@StaticR.albums_css@
      %script!src="http://code.jquery.com/jquery-1.4.2.min.js"
      %script!src=@StaticR.script_js@
    %body
      %h1 AwesomeQuiz.com
      %p AwesomeQuiz provides a wide ranging set of questions to tax your brain.  Pick from one of the following categories to get a randomly selected question.  Yes, it's crap at the moment, but that's because it's a beta.
      %ul
        %li Film Directors
        %li Film Actors
      %hr
      %p Written using 
          %a!href="http://docs.yesodweb.com/Yesod" Yesod Web Framework
  |]              

main :: IO ()
main = do
  let static = fileLookupDir "static/" typeByExt
  wDirector <- mkWhichDirector
  wActor <- mkWhichActor
  basicHandler 3000 $ QuizMaster static wDirector wActor