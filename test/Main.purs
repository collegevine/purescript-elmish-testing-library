module Test.Main where

import Prelude

import Debug (trace, traceM)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish (Dispatch, ReactElement, Transition, withTrace, (<?|))
import Elmish.Foreign (readForeign)
import Elmish.HTML.Styled as H
import Elmish.Test (find, tagName, testComponent, text, value, within, ($$), (>>))
import Elmish.Test.Events (change, clickOn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.String (shouldContain)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] spec

spec :: Spec Unit
spec =
  describe "Counter component" $
    it "Should render two buttons and a count" $
      testComponent (withTrace { init, view, update }) do
        -- within
        within ".t--dec" do
          text >>= shouldEqual "Dec"
          tagName >>= shouldEqual "BUTTON"

        -- chaining operations with >>
        find ".t--inc" >> text >>= shouldEqual "Inc"

        -- naming the element, then applying operation to it with $$
        count <- find ".t--count"
        strCount <- text $$ count
        strCount `shouldEqual` "0"

        -- click
        clickOn ".t--inc"
        find ".t--count" >> text >>= shouldEqual "1"

        -- change
        within "input" do
          value >>= shouldEqual "Harry"
          change "Frodo"
          value >>= shouldEqual "Frodo"

        text >>= (_ `shouldContain` "Hello, Frodo")

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
  , H.button_ "t--inc" { onClick: dispatch Inc } "Inc"
  , H.button_ "t--dec" { onClick: dispatch Dec } "Dec"

  , H.input_ ""
    { type: "text"
    , value: state.text
    , onChange: dispatch <?| \e -> readForeign e <#>
        \(x :: { target :: { value :: _ } }) -> TextChanged x.target.value
    }

  , H.span "" $ "Hello, " <> state.text
  ]
