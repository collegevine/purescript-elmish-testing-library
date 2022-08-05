module Elmish.Test.SpinWait where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay, error)
import Effect.Aff.Class (liftAff)
import Elmish.Test.State (class Testable)

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after a second.
waitWhile :: ∀ m. Testable m => m Boolean -> m Unit
waitWhile = waitWhile' (Milliseconds 1000.0)

-- | Performs active wait while the given condition is false. Times out with a
-- | crash after a second.
waitUntil :: ∀ m. Testable m => m Boolean -> m Unit
waitUntil = waitUntil' (Milliseconds 1000.0)

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after given time period.
waitWhile' :: ∀ m. Testable m => Milliseconds -> m Boolean -> m Unit
waitWhile' timeout f = waitUntil' timeout $ not <$> f

-- | Performs active wait while the given condition is true. Times out with a
-- | crash after given time period.
waitUntil' :: ∀ m. Testable m => Milliseconds -> m Boolean -> m Unit
waitUntil' (Milliseconds timeout) f = go timeout
  where
    go remaining = do
      when (remaining <= 0.0) $
        liftAff $ throwError $ error "Timeout expired"
      liftAff $ delay $ Milliseconds 1.0
      unlessM f $ go $ remaining - 1.0
