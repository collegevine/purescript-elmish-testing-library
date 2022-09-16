module Elmish.Test.Combinators where

import Prelude

import Control.Monad.Reader (local)
import Data.Traversable (traverse, traverse_)
import Elmish.Test.Discover (find)
import Elmish.Test.State (class Testable, TestState(..))
import Web.DOM (Element)

-- | Finds an element by the given selector and runs the given computation in
-- | the context of that element.
-- |
-- | Example:
-- |
-- |     describe "My component" $
-- |       it "should work" $
-- |         testComponent { init, view, update } do
-- |
-- |           within "section:nth-child(1)" do
-- |             find "h2" >> text >>= shouldEqual "First Section"
-- |             clickOn "button"
-- |
-- |           within "section:nth-child(2)" do
-- |             find "h2" >> text >>= shouldEqual "Second Section"
-- |             find "input[type=text]" >> change "Some Text"
-- |
within :: ∀ m a. Testable m => String -> m a -> m a
within selector f = do
  el <- find selector
  within' el f

infixl 8 within' as ##

-- | A more general version of `within` that accepts an `Element` instead of a
-- | CSS selector.
-- |
-- | In its operator form `##` this function can be used similarly to postfix
-- | function application, for example:
-- |
-- |     button <- find "button"
-- |     button ## click
-- |
within' :: ∀ m a. Testable m => Element -> m a -> m a
within' el = local \(TestState s) -> TestState s { current = el }

infixl 8 chainM as >>

-- | Used in its operator form `>>`, this function chains two DOM operations
-- | together, taking the output of the first operation and making it context of
-- | the second one. For example:
-- |
-- |     buttonInsideDiv <- find "div" >> find "button"
-- |     inputValue <- find "input" >> attr "value"
-- |
chainM :: ∀ m a. Testable m => m Element -> m a -> m a
chainM getEl f = do
  el <- getEl
  within' el f

infixl 8 chain as $$

-- | A flipped version of `within'`. In its operator form `$$` this function can
-- | be used similarly to function application, for example:
-- |
-- |     button <- find "button"
-- |     click $$ button
-- |
chain :: ∀ m a. Testable m => m a -> Element -> m a
chain = flip within'

-- | Runs the given computation multiple times, in the context of each of the
-- | given `Element`s.
-- |
-- |     findAll "button" >> forEach click
-- |
forEach :: ∀ m. Testable m => m Unit -> Array Element -> m Unit
forEach f = traverse_ \el -> f $$ el

-- | Runs the given computation multiple times, in the context of each of the
-- | given `Element`s, and returns their results as an array.
-- |
-- |     values <- findAll "input" >> mapEach (prop P.value)
-- |
mapEach :: ∀ m a. Testable m => m a -> Array Element -> m (Array a)
mapEach f = traverse \el -> f $$ el
