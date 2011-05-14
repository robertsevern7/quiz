{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
module StaticFiles where

import Yesod.Helpers.Static
-- | This generates easy references to files in the static directory at compile time.
--   The upside to this is that you have compile-time verification that referenced files
--   exist. However, any files added to your static directory during run-time can't be
--   accessed this way. You'll have to use their FilePath or URL to access them.
$(staticFiles "static")