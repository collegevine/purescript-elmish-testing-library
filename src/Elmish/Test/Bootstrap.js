import { GlobalRegistrator } from '@happy-dom/global-registrator';

// The `window` check is what lets the same code run in a browser, or against a
// DOM emulator that someone else initialized before importing this library.
export const ensureDom_ = () => {
  if (typeof window === "undefined") {
    GlobalRegistrator.register()
  }
}

// Registering the DOM happens at load time, and not only from `testComponent`,
// because React DOM decides at import time whether the environment supports
// native input events, and decides "no" when there is no DOM yet, which leaves
// `change` events unable to reach an `onChange` handler ever after. React DOM
// gets imported as part of loading this module (via `Elmish.HTML.DOM`), and ES
// modules are evaluated in import order with the foreign module first, so this
// is the last moment at which the DOM can still be put in place first.
//
// `testComponent` calls `ensureDom_` as well, which is redundant but load
// bearing: purs only imports a foreign module when the PureScript module has a
// `foreign import` to justify it, so dropping that call, and the declaration
// along with it, would stop this file from being loaded at all.
ensureDom_()
