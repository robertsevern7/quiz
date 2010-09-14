{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Freebase
import Text.JSON

-- Site specific data is stored here
data QuizMaster = QuizMaster {
      ajaxStatic :: Static
}

staticFiles "static/"

mkYesod "QuizMaster" [$parseRoutes|                      
  /       HomeR   GET
  /static StaticR Static ajaxStatic
|]

instance Yesod QuizMaster where
    approot _ = ""
   
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
    %p AwesomeQuiz provides a wide ranging set of questions to tax your brain.
    %hr
    %p Written using 
        %a!href="http://docs.yesodweb.com/Yesod" Yesod Web Framework
|]              

main :: IO ()
main = do
  let static = fileLookupDir "static/" typeByExt
  basicHandler 3000 $ QuizMaster static