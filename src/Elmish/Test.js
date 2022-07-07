import { GlobalRegistrator } from '@happy-dom/global-registrator';

export const ensureDom = () => {
  if (typeof window === "undefined") {
    GlobalRegistrator.register()
  }
}

export const innerText = e => e.innerText
