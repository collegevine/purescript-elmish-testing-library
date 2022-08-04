module Elmish.Test.Query where

import Prelude

import Data.Array (length, mapMaybe, null)
import Data.Maybe (fromMaybe)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Elmish.Test.State (class Testable, askCurrent, crash)
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
  current <- askCurrent
  liftEffect $
    querySelectorAll (QuerySelector selector) (DOM.toParentNode current)
    >>= NodeList.toArray
    <#> mapMaybe DOM.fromNode

exists :: ∀ m. Testable m => String -> m Boolean
exists selector = not null <$> findAll selector

text :: ∀ m. Testable m => m String
text = askCurrent >>= (liftEffect <<< runEffectFn1 innerText_)

tagName :: ∀ m. Testable m => m String
tagName = askCurrent <#> DOM.tagName

attr :: ∀ m. Testable m => String -> m String
attr name = askCurrent >>= \e -> liftEffect $ DOM.getAttribute name e <#> fromMaybe ""

value :: ∀ m. Testable m => m String
value = askCurrent >>= (liftEffect <<< runEffectFn1 value_)

foreign import innerText_ :: EffectFn1 Element String

foreign import value_ :: EffectFn1 Element String
