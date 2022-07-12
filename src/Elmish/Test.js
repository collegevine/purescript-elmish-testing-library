import { GlobalRegistrator } from '@happy-dom/global-registrator';
import { fireEvent } from '@testing-library/react'

export const ensureDom_ = () => {
  if (typeof window === "undefined") {
    GlobalRegistrator.register()
  }
}

export const innerText_ = e => e.innerText

export const value_ = e => e.value

export const fireEvent_ = (name, args, e) => {
  const evt = fireEvent[name]
  if (!evt) {
    throw `Unrecognized event name: ${name}`
  }

  evt(e, args)
}
