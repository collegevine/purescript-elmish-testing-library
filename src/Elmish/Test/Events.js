import { createEvent, fireEvent } from '@testing-library/dom'

// Some React event names don't correspond to a native event of the same name:
// React synthesizes them from other native events, and those are the ones that
// have to be dispatched for the React handler to run. The names on the right
// are `@testing-library/dom` names, which are camel-cased native ones.
const nativeEvents = {
  doubleClick: ["dblClick"],
  mouseEnter: ["mouseEnter", "mouseOver"],
  mouseLeave: ["mouseLeave", "mouseOut"],
  pointerEnter: ["pointerEnter", "pointerOver"],
  pointerLeave: ["pointerLeave", "pointerOut"],
  focus: ["focus", "focusIn"],
  blur: ["blur", "focusOut"],
  select: ["select", "keyUp"]
}

export const fireEvent_ = (name, args, e) => {
  const names = nativeEvents[name] ?? [name]

  for (const n of names) {
    if (!createEvent[n]) {
      throw `Unrecognized event name: ${name}`
    }
  }

  // React tracks selection only on the focused element.
  if (name === "select") {
    e.focus()
  }

  for (const n of names) {
    const event = createEvent[n](e, args)

    // `createEvent` passes the args to the event's constructor, which silently
    // drops every property that isn't part of that event interface (e.g.
    // `keyCode` on a keyboard event). Put those back, because the contract of
    // `fireEvent` is that the whole record ends up on the event object.
    // `defineProperty` rather than assignment, because most event properties
    // are prototype getters without a setter.
    for (const key of Object.keys(args)) {
      if (key !== "target" && event[key] !== args[key]) {
        Object.defineProperty(event, key, { configurable: true, enumerable: true, value: args[key] })
      }
    }

    fireEvent(e, event)
  }
}
