{-# LANGUAGE DeriveDataTypeable #-}
module Exception (
  QuizException(QuizException),
  message,
  internal
  ) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data QuizException = QuizException {
  message :: String,
  internal :: String
} deriving (Show,Typeable)

instance Exception QuizException
