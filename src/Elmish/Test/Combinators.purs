module Elmish.Test.Combinators where

import Prelude

import Control.Monad.Reader (local)
import Elmish.Test.Query (find)
import Elmish.Test.State (class Testable, TestState(..))
import Web.DOM (Element)

within :: ∀ m a. Testable m => String -> m a -> m a
within selector f = do
  el <- find selector
  within' el f

infixl 8 within' as ##

within' :: ∀ m a. Testable m => Element -> m a -> m a
within' el = local \(TestState s) -> TestState s { current = el }

infixl 8 chainM as >>

chainM :: ∀ m a. Testable m => m Element -> m a -> m a
chainM getEl f = do
  el <- getEl
  within' el f

infixl 8 chain as $$

chain :: ∀ m a. Testable m => m a -> Element -> m a
chain = flip within'
