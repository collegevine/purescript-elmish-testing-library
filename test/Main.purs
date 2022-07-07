module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Elmish (Dispatch, ReactElement, Transition)
import Elmish.HTML.Styled as H
import Elmish.Test (find, tagName, testComponent, text, within, ($$), (>>))
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

        -- naming the element, then applying operation to it with $$
        count <- find ".t--count"
        strCount <- text $$ count
        strCount `shouldEqual` "0"



type State = { count :: Int }

data Message = Inc | Dec

init :: Transition Message State
init = pure { count: 0 }

update :: State -> Message -> Transition Message State
update state Inc = pure state { count = state.count + 1 }
update state Dec = pure state { count = state.count - 1 }

view :: State -> Dispatch Message -> ReactElement
view state dispatch =
  H.div ""
  [ H.div "t--count" $ show state.count
  , H.button_ "t--inc" { onClick: dispatch Inc } "Inc"
  , H.button_ "t--dec" { onClick: dispatch Dec } "Dec"
  ]
