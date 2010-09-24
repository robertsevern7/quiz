{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Freebase
import Films
import Logic

import Text.JSON

-- Site specific data is stored here
data QuizMaster = QuizMaster {
      ajaxStatic :: Static
}

staticFiles "static/"

mkYesod "QuizMaster" [$parseRoutes|                      
  /       HomeR   GET
  /static StaticR Static ajaxStatic
  /actors ActorsR GET
  /films  FilmsR GET
|]

instance Yesod QuizMaster where
    approot _ = ""

getActorsR :: GHandler sub QuizMaster RepHtml
getActorsR = undefined

getFilmsR :: GHandler sub QuizMaster RepHtml
getFilmsR = undefined
   
getHomeR :: GHandler sub QuizMaster RepHtml
getHomeR = hamletToRepHtml [$hamlet|
%html
  %head
    %title Quiz Master
    %link!rel="stylesheet"!href=@StaticR.albums_css@
    %script!src="http://code.jquery.com/jquery-1.4.2.min.js"
    %script!src=@StaticR.script_js@
  %body
    %h1 AwesomeQuiz.com
    %p AwesomeQuiz provides a wide ranging set of questions to tax your brain.  Pick from one of the following categories to get a randomly selected question.
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
  basicHandler 3000 $ QuizMaster static