import React from 'react';

export const act_ = effect => {
  // React 17 renders synchronously, so `act` is not necessary, which is why it
  // doesn't exist. React 18+, however, renders asynchronously, so `act` is
  // necessary to ensure that all updates are flushed before the test continues.
  // However, there is a hole: `act` is only available since React 18.3, so for
  // React 18.0-18.2, there is no solution, and we throw.
  if (typeof React.act !== "function") {
    if (!/^1[67]\./.test(React.version || "")) {
      throw `Elmish testing library needs React 17 or React 18.3+, but this is React ${React.version}`
    }
    effect()
    return
  }

  // `act` complains unless the environment declares itself an "act
  // environment", but that same flag also makes React complain about every
  // update that happens outside of `act` - which here means every update
  // produced by an Elmish command, since those run asynchronously. Hence the
  // flag is only on for the duration of the call.
  const wasActEnvironment = globalThis.IS_REACT_ACT_ENVIRONMENT
  globalThis.IS_REACT_ACT_ENVIRONMENT = true
  try {
    React.act(effect)
  } finally {
    globalThis.IS_REACT_ACT_ENVIRONMENT = wasActEnvironment
  }
}
