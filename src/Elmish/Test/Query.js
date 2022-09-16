export const outerHTML_ = e => e.outerHTML || ""
export const prop_ = (name, e) => e[name] || null // converting `undefined` to `null` so it can be handled via `Nullable`
