module Elmish.Test.Events where

import Prelude

import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Elmish.Foreign (class CanPassToJavaScript)
import Elmish.Test.Combinators ((>>))
import Elmish.Test.Query (find)
import Elmish.Test.State (class Testable, askCurrent)
import Web.DOM (Element)

fireEvent :: ∀ m r. Testable m => CanPassToJavaScript (Record r) => String -> Record r -> m Unit
fireEvent name args = askCurrent >>= \e ->
  liftEffect $ runEffectFn3 fireEvent_ name args e

click :: ∀ m. Testable m => m Unit
click = fireEvent "click" {}

clickOn :: ∀ m. Testable m => String -> m Unit
clickOn selector = find selector >> click

change :: ∀ m. Testable m => String -> m Unit
change value = fireEvent "change" { target: { value } }

foreign import fireEvent_ :: ∀ args. EffectFn3 String args Element Unit
