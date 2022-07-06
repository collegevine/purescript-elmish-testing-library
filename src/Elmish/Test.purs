module Elmish.Test where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (class MonadReader, ReaderT, ask, local, runReaderT)
import Data.Array (length, mapMaybe)
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Elmish (ComponentDef, construct)
import Elmish.React as React
import Web.DOM (Element)
import Web.DOM.Document (createElement)
import Web.DOM.Element as DOM
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

newtype TestState = TestState
  { root :: Element
  , current :: Element
  }

class (MonadReader TestState m, MonadEffect m) <= Testable m
instance MonadEffect m => Testable (ReaderT TestState m)

testComponent :: forall m a msg state. MonadEffect m => ComponentDef msg state -> ReaderT TestState m a -> m a
testComponent def go = do
  liftEffect ensureJsDom
  root <- liftEffect $ window >>= document <#> toDocument >>= createElement "div"
  reactEl <- liftEffect $ construct def
  liftEffect $ React.render reactEl root
  runReaderT go $ TestState { root, current: root }

find :: forall m. Testable m => String -> m Element
find selector =
  findAll selector >>= case _ of
    [el] -> pure el
    els -> crash $ "Expected to find one element matching '" <> selector <> "', but found " <> show (length els)

findAll :: forall m. Testable m => String -> m (Array Element)
findAll selector = do
  current <- askCurrent
  liftEffect $
    querySelectorAll (QuerySelector selector) (DOM.toParentNode current)
    >>= NodeList.toArray
    <#> mapMaybe DOM.fromNode

within :: forall m. Testable m => Element -> m Unit -> m Unit
within el = local \(TestState s) -> TestState s { current = el }


text :: forall m. Testable m => m String
text = askCurrent >>= (liftEffect <<< runEffectFn1 innerText)

tagName :: forall m. Testable m => m String
tagName = askCurrent <#> DOM.tagName

debug :: forall m. Testable m => m String
debug = pure ""

attr :: forall m. Testable m => String -> m String
attr name = askCurrent >>= \e -> liftEffect $ DOM.getAttribute name e <#> fromMaybe ""

askCurrent :: forall m. Testable m => m Element
askCurrent = ask <#> \(TestState s) -> s.current

crash :: forall m a. Testable m => String -> m a
crash = liftEffect <<< throwError <<< error

foreign import innerText :: EffectFn1 Element String

foreign import ensureJsDom :: Effect Unit
