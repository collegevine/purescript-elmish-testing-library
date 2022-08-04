module Elmish.Test.State where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader, ReaderT, ask)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (error)
import Web.DOM (Element)

newtype TestState = TestState
  { root :: Element
  , current :: Element
  }

class (MonadReader TestState m, MonadAff m) <= Testable m
instance MonadAff m => Testable (ReaderT TestState m)

currentNode :: ∀ m. Testable m => m Element
currentNode = ask <#> \(TestState s) -> s.current

crash :: ∀ m a. Testable m => String -> m a
crash = liftAff <<< throwError <<< error
