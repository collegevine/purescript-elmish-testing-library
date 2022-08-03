import { GlobalRegistrator } from '@happy-dom/global-registrator';
import { Simulate } from 'react-dom/test-utils.js'

export const ensureDom_ = () => {
  if (typeof window === "undefined") {
    GlobalRegistrator.register()
  }
}

export const innerText_ = e => e.innerText

export const value_ = e => e.value

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
