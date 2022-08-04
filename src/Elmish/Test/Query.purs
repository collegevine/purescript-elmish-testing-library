module Elmish.Test.Query where

import Prelude

import Data.Array (length, mapMaybe, null)
import Data.Maybe (fromMaybe)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Elmish.Test.State (class Testable, currentNode, crash)
import Web.DOM (Element)
import Web.DOM.Element as DOM
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)

find :: ∀ m. Testable m => String -> m Element
find selector =
  findAll selector >>= case _ of
    [el] -> pure el
    els -> crash $ "Expected to find one element matching '" <> selector <> "', but found " <> show (length els)

findAll :: ∀ m. Testable m => String -> m (Array Element)
findAll selector = do
  current <- currentNode
  liftEffect $
    querySelectorAll (QuerySelector selector) (DOM.toParentNode current)
    >>= NodeList.toArray
    <#> mapMaybe DOM.fromNode

exists :: ∀ m. Testable m => String -> m Boolean
exists selector = not null <$> findAll selector

text :: ∀ m. Testable m => m String
text = currentNode >>= (liftEffect <<< runEffectFn1 innerText_)

html :: ∀ m. Testable m => m String
html = currentNode >>= (liftEffect <<< runEffectFn1 innerHTML_)

tagName :: ∀ m. Testable m => m String
tagName = currentNode <#> DOM.tagName

attr :: ∀ m. Testable m => String -> m String
attr name = currentNode >>= \e -> liftEffect $ DOM.getAttribute name e <#> fromMaybe ""

value :: ∀ m. Testable m => m String
value = currentNode >>= (liftEffect <<< runEffectFn1 value_)

foreign import innerText_ :: EffectFn1 Element String

foreign import innerHTML_ :: EffectFn1 Element String

foreign import value_ :: EffectFn1 Element String
