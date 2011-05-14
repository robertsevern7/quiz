{-# LANGUAGE DeriveDataTypeable #-}
module Exception (
  QuizException(QuizException),
  message,
  internal
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import Data.Text (Text)

data QuizException = QuizException {
  message :: Text,
  internal :: Text
} deriving (Show,Typeable)

instance Exception QuizException
