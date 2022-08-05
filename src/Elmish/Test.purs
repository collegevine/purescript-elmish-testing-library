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
-- |           findAll "a" >>= traverse_ \anchor ->
-- |             anchor ## attr "href" >>= shouldEqual "http://foo.bar"
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
-- | Finally, if you have an `Element` value on your hands, you can apply a
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
module Elmish.Test
  ( module Elmish.Test.Bootstrap
  , module Elmish.Test.Combinators
  , module Elmish.Test.Events
  , module Elmish.Test.Query
  , module Elmish.Test.SpinWait
  ) where

import Elmish.Test.Bootstrap (testComponent, testElement)
import Elmish.Test.Combinators (chain, chainM, within, within', (##), ($$), (>>))
import Elmish.Test.Events (change, click, clickOn, fireEvent)
import Elmish.Test.Query (attr, exists, find, findAll, html, tagName, text, value)
import Elmish.Test.SpinWait (waitUntil, waitUntil', waitWhile, waitWhile')
