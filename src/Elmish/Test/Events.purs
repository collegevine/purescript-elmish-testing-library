module Elmish.Test.Events where

import Prelude

import Elmish.Test (class Testable, find, fireEvent, (>>))

click :: ∀ m. Testable m => m Unit
click = fireEvent "click" {}

clickOn :: ∀ m. Testable m => String -> m Unit
clickOn selector = find selector >> click

change :: ∀ m. Testable m => String -> m Unit
change value = fireEvent "change" { target: { value } }
