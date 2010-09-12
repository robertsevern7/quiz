{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell #-}
import Yesod
import Yesod.Helpers.Static

import Freebase
import Text.JSON

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
    %title Album Lister
    %link!rel="stylesheet"!href=@StaticR.albums_css@
    %script!src="http://code.jquery.com/jquery-1.4.2.min.js"
    %script!src=@StaticR.script_js@
  %body
    %h1 Album Lister
    %p Enter the name of a band:
    %input!type=text!onchange="listAlbums(this.value)"
    %hr
    #output
    %hr
    %p Written using 
        %a!href="http://docs.yesodweb.com/Yesod" Yesod Web Framework
|]              
{-
getAlbumsR :: String -> GHandler sub master RepJson
getAlbumsR band = do
  albumsResult <- liftIO $ getAlbumList band
  case albumsResult of
    (Ok albums) -> jsonToRepJson $ jsonMap [("name", jsonList $ map jsonScalar albums)]
    (Error _)   -> jsonToRepJson $ jsonMap [("error", jsonScalar "Unknown band")]
-}

main :: IO ()
main = do
  let static = fileLookupDir "static/" typeByExt
  basicHandler 3000 $ QuizMaster static