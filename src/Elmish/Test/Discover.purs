module Elmish.Test.Discover
  ( childAt
  , children
  , find
  , findAll
  , findFirst
  , findNth
  )
  where

import Prelude

import Data.Array (fold, length, mapMaybe, (!!))
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
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

-- | Finds the first element out of possibly many matching the given selector.
-- | If there are no elements matching the selector, throws an exception.
findFirst :: ∀ m. Testable m => String -> m Element
findFirst = findNth 0


-- | Finds the n-th (zero-based) element out of possibly many matching the given
-- | selector. If there are no elements matching the selector, throws an
-- | exception.
findNth :: ∀ m. Testable m => Int -> String -> m Element
findNth idx selector =
  findAll selector >>= \all -> case all !! idx of
    Just el -> pure el
    Nothing -> crash $ fold
      [ "Expected to find "
      , show idx
      , "th element matching '"
      , selector
      , "', but there are only "
      , show (length all)
      , " elements"
      ]

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
