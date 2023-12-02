module Test.Main where

import Prelude

import Data.Array (length)
import Data.Traversable (for, for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish (Dispatch, ReactElement, Transition, (<|))
import Elmish.Component (ComponentName(..), wrapWithLocalState)
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Test (attr, find, findAll, nearestEnclosingReactComponentName, prop, tagName, testComponent, text, within, (##), ($$), (>>))
import Elmish.Test.DomProps as P
import Elmish.Test.Events (change, click, clickOn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

spec :: Spec Unit
spec =
  describe "Counter component" $
    it "Should render two buttons and a count" $
      testComponent { init, view, update } do
        -- within
        within ".t--dec" do
          text >>= shouldEqual "Dec"
          tagName >>= shouldEqual "BUTTON"

        -- chaining operations with >>
        find ".t--inc" >> text >>= shouldEqual "Inc"

        -- naming the element, then applying operation to it with $$ and ##
        count <- find ".t--count"
        strCount <- text $$ count
        strCount `shouldEqual` "0"
        countTag <- count ## tagName
        countTag `shouldEqual` "DIV"

        -- click
        clickOn ".t--inc"
        find ".t--count" >> text >>= shouldEqual "1"

        -- change
        within "input" do
          prop P.value >>= shouldEqual "Harry"
          change "Frodo"
          prop P.value >>= shouldEqual "Frodo"

        text >>= shouldEqual "1IncDecHello, FrodoFoo"

        -- findAll
        buttons <- findAll "button"
        length buttons `shouldEqual` 2

        names <- for buttons \b -> attr "class" $$ b
        names `shouldEqual` ["t--inc", "t--dec"]

        for_ buttons \b -> click $$ b
        find ".t--count" >> text >>= shouldEqual "1"

        -- React component names
        nearestEnclosingReactComponentName >>= shouldEqual "ElmishRoot"

        within ".t--standalone" do
          text >>= shouldEqual "Foo"
          nearestEnclosingReactComponentName >>= shouldEqual "Elmish_StandaloneComponent"

type State = { count :: Int, text :: String }

data Message = Inc | Dec | TextChanged String

init :: Transition Message State
init = pure { count: 0, text: "Harry" }

update :: State -> Message -> Transition Message State
update state Inc = pure state { count = state.count + 1 }
update state Dec = pure state { count = state.count - 1 }
update state (TextChanged s) = pure state { text = s }

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div ""
  [ H.div "t--count" $ show state.count
  , H.button_ "t--inc" { onClick: dispatch <| Inc } "Inc"
  , H.button_ "t--dec" { onClick: dispatch <| Dec } "Dec"

  , H.input_ ""
    { type: "text"
    , value: state.text
    , onChange: dispatch <| TextChanged <<< E.inputText
    }

  , H.span "" $ "Hello, " <> state.text

  , standaloneComponent { text: "Foo" }
  ]

standaloneComponent :: { text :: String } -> ReactElement
standaloneComponent = wrapWithLocalState (ComponentName "StandaloneComponent")
  \{ text } ->
    { init: pure unit
    , update: \_ _ -> pure unit
    , view: \_ _ -> H.span "t--standalone" text
    }
