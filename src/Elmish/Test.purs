module Elmish.Test where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, local, runReaderT)
import Data.Array (length, mapMaybe)
import Data.Maybe (fromMaybe)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, EffectFn3, runEffectFn1, runEffectFn3)
import Elmish (ComponentDef, ReactElement, construct)
import Elmish.Foreign (class CanPassToJavaScript)
import Elmish.React as React
import Web.DOM (Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element as DOM
import Web.DOM.Node (appendChild)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement as H
import Web.HTML.Window (document)

newtype TestState = TestState
  { root :: Element
  , current :: Element
  }

class (MonadReader TestState m, MonadEffect m) <= Testable m
instance MonadEffect m => Testable (ReaderT TestState m)

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

find :: ∀ m. Testable m => String -> m Element
find selector =
  findAll selector >>= case _ of
    [el] -> pure el
    els -> crash $ "Expected to find one element matching '" <> selector <> "', but found " <> show (length els)

findAll :: ∀ m. Testable m => String -> m (Array Element)
findAll selector = do
  current <- askCurrent
  liftEffect $
    querySelectorAll (QuerySelector selector) (DOM.toParentNode current)
    >>= NodeList.toArray
    <#> mapMaybe DOM.fromNode

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


text :: ∀ m. Testable m => m String
text = askCurrent >>= (liftEffect <<< runEffectFn1 innerText_)

tagName :: ∀ m. Testable m => m String
tagName = askCurrent <#> DOM.tagName

attr :: ∀ m. Testable m => String -> m String
attr name = askCurrent >>= \e -> liftEffect $ DOM.getAttribute name e <#> fromMaybe ""

value :: ∀ m. Testable m => m String
value = askCurrent >>= (liftEffect <<< runEffectFn1 value_)

fireEvent :: ∀ m r. Testable m => CanPassToJavaScript (Record r) => String -> Record r -> m Unit
fireEvent name args = askCurrent >>= \e ->
  liftEffect $ runEffectFn3 fireEvent_ name args e

debug :: ∀ m. Testable m => m String
debug = pure ""

--
--
--

askCurrent :: ∀ m. Testable m => m Element
askCurrent = ask <#> \(TestState s) -> s.current

crash :: ∀ m a. Testable m => String -> m a
crash = liftEffect <<< throwError <<< error

foreign import innerText_ :: EffectFn1 Element String

foreign import value_ :: EffectFn1 Element String

foreign import fireEvent_ :: ∀ args. EffectFn3 String args Element Unit

foreign import ensureDom_ :: Effect Unit
