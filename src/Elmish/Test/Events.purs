module Elmish.Test.Events
  ( change
  , click
  , clickOn
  , fireEvent
  ) where

import Prelude

import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Elmish.Foreign (class CanPassToJavaScript)
import Elmish.Test.Combinators ((>>))
import Elmish.Test.Discover (find)
import Elmish.Test.State (class Testable, currentNode)
import Web.DOM (Element)

-- | Simulates a React-friendly event with the given name and given args, firing
-- | it on the current-context element. The second argument is a record of
-- | values that gets merged into the event object.
-- |
-- | For example:
-- |
-- |     find "button" >> fireEvent "click" {}
-- |     find "input" >> fireEvent "change" { target: { value: "some text" } }
-- |     find "input" >> fireEvent "keyDown" { key: "Enter", keyCode: 13, which: 13 }
-- |
-- | This function uses the `Simulate` function from `react-dom/test-utils`. See
-- | https://reactjs.org/docs/test-utils.html#simulate
-- |
-- | If the arguments record contains a field `target`, which in turn contains a
-- | field `value`, the value of that field is assigned to the `value` property
-- | of the current-context element before firing the event. This special case
-- | is intended to support events targeted at input fields, such as `change` or
-- | `input`, where the event handler usually tries to access the
-- | `event.target.value` field.
-- |
fireEvent :: ∀ m r. Testable m => CanPassToJavaScript (Record r) => String -> Record r -> m Unit
fireEvent name args = currentNode >>= \e ->
  liftEffect $ runEffectFn3 fireEvent_ name args e

-- | A convenience specialization of `fireEvent`, simulating the `click` event.
-- |
-- |     find "button" >> click
-- |
click :: ∀ m. Testable m => m Unit
click = fireEvent "click" {}

-- | A convenience function, finding an element by CSS selector and simulating
-- | the `click` event on it.
-- |
-- |     clickOn "button.t--my-button"
-- |
clickOn :: ∀ m. Testable m => String -> m Unit
clickOn selector = find selector >> click

-- | A convenience specialization of `fireEvent`, simulating the `change` event
-- | with the given value.
-- |
-- |     find "input" >> change "some text"
-- |
change :: ∀ m. Testable m => String -> m Unit
change value = fireEvent "change" { target: { value } }

foreign import fireEvent_ :: ∀ args. EffectFn3 String args Element Unit
