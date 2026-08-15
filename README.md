# Elmish Testing Library

A library for testing [Elmish](https://github.com/collegevine/purescript-elmish)
components on live DOM. Can be run in the browser on native DOM or under Node,
in which case it will emulate the DOM via the [Happy DOM
library](https://github.com/capricorn86/happy-dom). A different DOM emulator can
also be used as long as it's initialized before this library is imported: React
DOM works out what the environment supports when it loads, so a DOM that shows
up later leaves its event handling in a degraded state.

Events are dispatched with
[`@testing-library/dom`](https://testing-library.com/docs/dom-testing-library/intro),
so that package needs to be installed alongside `react` and `react-dom`.

See documentation [on Pursuit](https://pursuit.purescript.org/packages/purescript-elmish-testing-library/docs/Elmish.Test).
