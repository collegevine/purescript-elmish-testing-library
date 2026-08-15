-- | Internal. Not part of the public API, and not re-exported from
-- | `Elmish.Test`.
module Elmish.Test.React
  ( act
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

-- | Runs the given effect and then lets React finish whatever rendering that
-- | effect caused, so that the DOM is up to date by the time the effect
-- | returns. Without this, React would apply the updates on a later tick, and
-- | the very next line of a test would be looking at a stale DOM. On React 17,
-- | where rendering is synchronous to begin with, this is just the effect.
act :: Effect Unit -> Effect Unit
act = runEffectFn1 act_

foreign import act_ :: EffectFn1 (Effect Unit) Unit
