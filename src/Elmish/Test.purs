-- | Elmish Testing Library tries to roughly follow the general API shape of
-- | libraries like JS Selenium or Ruby/Rails Capybara:
-- |
-- | * `testComponent` or `testElement` to mount the component and start a test.
-- | * `find "button.some-class"` - to find a single element (will crash when
-- |   not found or found more than one).
-- | * `findAll "div.main-content"` - to find zero or multiple elements.
-- | * Query elements via `text`, `html`, `attr`, `tagName`, and so on.
-- | * Simulate events via `fireEvent` or convenience facades like `click`,
-- |   `clickOn`, or `change`
-- | * Chain operations via `>>`, like `find "button" >> text` or `find "div" >>
-- |   find "a" >> attr "href"`
-- | * `within` to zoom into an element and run some code in its context.
-- | * `forEach` and `mapEach` to run computations in the context of multiple
-- |   elements in sequence.
-- |
-- | An illustrating example:
-- |
-- | ```haskell
-- | spec :: Spec Unit
-- | spec =
-- |   describe "My component" $
-- |     it "illustrates how to write a test" $
-- |       testComponent { init, view, update } do
-- |         find "h1" >> text >>= shouldEqual "Hello"
-- |
-- |         find "input[type=text]" >> change "Some text in the textbox"
-- |         find "button" >> click
-- |
-- |         within "div.wrapper" do
-- |           text >>= (_ `shouldContain` "Some text inside a wapper")
-- |           findAll "a" >> forEach do
-- |             attr "href" >>= shouldEqual "http://foo.bar"
-- | ```
-- |
-- | Every DOM-manipulating operation (e.g. `find`, `text`, `fireEvent`, and so
-- | on) doesn't take a DOM node as a parameter, but instead runs in a monad
-- | that always has a "current" (or "focused") node in context. All operations
-- | always apply to that "current" node.
-- |
-- | The current node can be changed (or "refocused", or "zoomed") via `within`
-- | or `within'`. These functions find (or take) another node and run a
-- | computation with that node as "current", thus making all operations inside
-- | a `within` apply to it.
-- |
-- | In operator form `>>`, these functions allow "chaining" operations
-- | together, as in:
-- |
-- |     find "button" >> click
-- |
-- |     -- equivalent to:
-- |     b <- find "button"
-- |     within' b click
-- |
-- | The difference is admittedly only cosmetic, but it allows for nice looking
-- | code, like:
-- |
-- |     -- click a button within the 6th subnode of the root <div>
-- |     find "div" >> childAt 5 >> find "button" >> click
-- |
-- | If you have an `Element` value on your hands, you can apply a
-- | DOM-manipulating operation to it via `$$` (prefix) or `##` (postfix), for
-- | example:
-- |
-- |     button <- find "button"
-- |     click $$ button  -- equivalent to: within' button click
-- |     button ## click  -- equivalent to: within' button click
-- |
-- | These operators are necessary, because operations can't be applied as
-- | functions (e.g. `click button`), since they don't take a node as parameter.
-- |
-- | Finally, if you have an array of elements (e.g. obtained via `findAll`),
-- | you could iterate over them with `traverse`, like:
-- |
-- |     findAll "button" >>= traverse_ \button -> button ## click
-- |     findAll "a" >>= traverse_ \a ->
-- |       within' a do
-- |         attr "href" >>= shouldEqual "http://foo"
-- |         text >>= shouldEqual "Click me"
-- |
-- | But it's more convenient to use special helper functions `forEach` and
-- | `mapEach` instead. These functions run the given computation in the context
-- | of every element in the array:
-- |
-- |     findAll "button" >> forEach click
-- |     findAll "a" >> forEach do
-- |       attr "href" >>= shouldEqual "http://foo"
-- |       text >>= shouldEqual "Click me"
-- |
-- | Or even:
-- |
-- |     findAll "a" >> mapEach text >>= shouldEqual ["Click me", "Click me"]
-- |
module Elmish.Test
  ( module Elmish.Test.Bootstrap
  , module Elmish.Test.Combinators
  , module Elmish.Test.Discover
  , module Elmish.Test.Events
  , module Elmish.Test.Query
  , module Elmish.Test.SpinWait
  ) where

import Elmish.Test.Bootstrap (testComponent, testElement)
import Elmish.Test.Combinators (chain, chainM, forEach, mapEach, within, within', (##), ($$), (>>))
import Elmish.Test.Discover (childAt, children, find, findAll, findFirst, findNth)
import Elmish.Test.Events (change, click, clickOn, fireEvent)
import Elmish.Test.Query (attr, count, exists, html, nearestEnclosingReactComponentName, prop, tagName, text)
import Elmish.Test.SpinWait (waitUntil, waitUntil', waitWhile, waitWhile')
