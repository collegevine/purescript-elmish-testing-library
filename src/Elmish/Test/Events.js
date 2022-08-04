import { Simulate } from 'react-dom/test-utils.js'

export const fireEvent_ = (name, args, e) => {
  const evt = Simulate[name]
  if (!evt) {
    throw `Unrecognized event name: ${name}`
  }

  if (args && args.target && typeof args.target.value !== "undefined") {
    e.value = args.target.value
  }

  evt(e, args)
}
