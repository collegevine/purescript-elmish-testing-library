module Elmish.Test.Query
  ( attr
  , count
  , exists
  , html
  , nearestEnclosingReactComponentName
  , prop
  , tagName
  , text
  )
  where

import Prelude

import Data.Array (length, null)
import Data.Maybe (fromMaybe)
import Data.Nullable as N
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Elmish.Test.Discover (findAll)
import Elmish.Test.DomProps (class DomPropType, DomProp, defaultValue)
import Elmish.Test.State (class Testable, currentNode)
import Web.DOM (Element)
import Web.DOM.Element as DOM

-- | Returns `true` if at least one element exists matching the given CSS
-- | selector.
exists :: ∀ m. Testable m => String -> m Boolean
exists selector = not null <$> findAll selector

-- | Returns the number of elements within the current context that match the
-- | given selector.
count :: ∀ m. Testable m => String -> m Int
count selector = length <$> findAll selector

-- | Returns full inner text of the current-context element.
text :: ∀ m. Testable m => m String
text = currentNode >>= (liftEffect <<< runEffectFn1 innerText_)

-- | Returns HTML representation of the current-context element.
html :: ∀ m. Testable m => m String
html = currentNode >>= (liftEffect <<< runEffectFn1 outerHTML_)

-- | Returns the tag name of the current-context element.
tagName :: ∀ m. Testable m => m String
tagName = currentNode <#> DOM.tagName

-- | Returns the given attribute of the current-context element.
attr :: ∀ m. Testable m => String -> m String
attr name = currentNode >>= \e -> liftEffect $ DOM.getAttribute name e <#> fromMaybe ""

-- | Returns the given property of the current-context element.
-- |
-- |     import Elmish.Test.DomProps as P
-- |
-- |     find "input" >> prop P.value >>= shouldEqual "hello"
-- |
prop :: ∀ m a. Testable m => DomPropType a => DomProp a -> m a
prop name = currentNode >>= \e -> liftEffect $
  runEffectFn2 prop_ name e <#> N.toMaybe <#> fromMaybe defaultValue

nearestEnclosingReactComponentName :: ∀ m. Testable m => m String
nearestEnclosingReactComponentName = currentNode >>= \e -> liftEffect $ runEffectFn1 reactComponentName_ e

foreign import innerText_ :: EffectFn1 Element String

foreign import outerHTML_ :: EffectFn1 Element String

foreign import prop_ :: ∀ a. EffectFn2 (DomProp a) Element (N.Nullable a)

foreign import reactComponentName_ :: EffectFn1 Element String
