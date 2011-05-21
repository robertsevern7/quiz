{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Model where

import Yesod
import Data.Text (Text)

-- You can define all of your database entities here. You can find more
-- information on persistent and how to declare entities at:
-- http://docs.yesodweb.com/book/persistent/
share [mkPersist, mkMigrate "migrateAll"] $(persistFile "config/models")
