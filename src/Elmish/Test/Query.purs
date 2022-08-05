module Elmish.Test.Query
  ( attr
  , childAt
  , children
  , exists
  , find
  , findAll
  , html
  , prop
  , tagName
  , text
  )
  where

import Prelude

import Data.Array (fold, length, mapMaybe, null, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Elmish.Test.DomProps (DomProp)
import Elmish.Test.State (class Testable, crash, currentNode)
import Web.DOM (Element)
import Web.DOM.Element as DOM
import Web.DOM.Node (childNodes)
import Web.DOM.NodeList as NodeList
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)

-- | Finds exactly one element by CSS selector. If the selector matches zero
-- | elements or more than one, this function will throw an exception.
-- |
-- |     find "button" >> click
-- |
find :: ∀ m. Testable m => String -> m Element
find selector =
  findAll selector >>= case _ of
    [el] -> pure el
    els -> crash $ "Expected to find one element matching '" <> selector <> "', but found " <> show (length els)

-- | Finds zero or more elements by CSS selector.
-- |
-- |   findAll "button" >>= traverse_ \b -> click $$ b
-- |
-- |   divs <- find "div"
-- |   length divs `shouldEqual` 10
-- |
findAll :: ∀ m. Testable m => String -> m (Array Element)
findAll selector = do
  current <- currentNode
  liftEffect $
    querySelectorAll (QuerySelector selector) (DOM.toParentNode current)
    >>= NodeList.toArray
    <#> mapMaybe DOM.fromNode

-- | Returns all immediate child elements of the current-context element.
-- |
-- |     find "div" >> children >>= traverse_ \child ->
-- |       tag <- tagName
-- |       when (tag == "BUTTON")
-- |         click
-- |
children :: ∀ m. Testable m => m (Array Element)
children = do
  current <- currentNode
  liftEffect $
    childNodes (DOM.toNode current)
    >>= NodeList.toArray
    <#> mapMaybe DOM.fromNode

-- | Within the current-context element, finds a child element at the given
-- | index. Crashes if the is no child with the given index.
childAt :: ∀ m. Testable m => Int -> m Element
childAt idx = do
  cs <- children
  case cs !! idx of
    Just e ->
      pure e
    Nothing ->
      crash $ fold
        [ "Expected to find a child element at index "
        , show idx
        , ", but there are only "
        , show (length cs)
        , " children"
        ]

-- | Returns `true` if at least one element exists matching the given CSS
-- | selector.
exists :: ∀ m. Testable m => String -> m Boolean
exists selector = not null <$> findAll selector

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
prop :: ∀ m a. Testable m => DomProp a -> m a
prop name = currentNode >>= \e -> liftEffect $ runEffectFn2 prop_ name e

foreign import innerText_ :: EffectFn1 Element String

foreign import outerHTML_ :: EffectFn1 Element String

foreign import prop_ :: ∀ a. EffectFn2 (DomProp a) Element a
