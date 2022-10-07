import { GlobalRegistrator } from '@happy-dom/global-registrator';

export const ensureDom_ = () => {
  if (typeof window === "undefined") {
    GlobalRegistrator.register()
  }
}
