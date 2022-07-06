import { GlobalRegistrator } from '@happy-dom/global-registrator';

GlobalRegistrator.register();

export const done = () => GlobalRegistrator.unregister()
