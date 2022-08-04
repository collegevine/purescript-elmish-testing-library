module Elmish.Test.Bootstrap where

import Prelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Elmish (ComponentDef, ReactElement, construct)
import Elmish.React as React
import Elmish.Test.State (class Testable, TestState(..))
import Web.DOM.Document (createElement)
import Web.DOM.Element as DOM
import Web.DOM.Node (appendChild)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as H
import Web.HTML.Window (document)

testComponent :: ∀ m a msg state. MonadEffect m => ComponentDef msg state -> ReaderT TestState m a -> m a
testComponent def go = do
  root <- liftEffect mount
  runReaderT go $ TestState { root, current: root }
  where
    mount = do
      ensureDom_

      doc <- window >>= document
      root <- doc # toDocument # createElement "div"
      doc # body >>= traverse_ \theBody ->
        appendChild (DOM.toNode root) (H.toNode theBody)

      reactEl <- construct def
      React.render reactEl root

      pure root

testElement :: ∀ m a. MonadEffect m => ReactElement -> ReaderT TestState m a -> m a
testElement element =
  testComponent { init: pure unit, view: \_ _ -> element, update: \_ _ -> pure unit }

debug :: ∀ m. Testable m => m String
debug = pure ""

foreign import ensureDom_ :: Effect Unit
