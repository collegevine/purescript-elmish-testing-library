module Test.Main where

import Prelude

import Debug (traceM)
import Effect (Effect)
import Elmish (Dispatch, ReactElement, Transition)
import Elmish.HTML.Styled as H
import Elmish.Test (find, testComponent, text, within)
import Test.Spec.Assertions (shouldEqual)

main :: Effect Unit
main = do
  traceM "foo"
  -- testComponent { init, view, update } do
  --   traceM "bar"
  --   el <- find ".t--inc"
  --   traceM "qux"
  --   within el $
  --     text >>= shouldEqual "Inc"
  traceM "baz"
--  done
  traceM "dd"


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


foreign import done :: Effect Unit
