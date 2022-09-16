import registerDom from 'global-jsdom'

export const ensureDom_ = () => {
  if (typeof window === "undefined") {
    registerDom()
  }
}
