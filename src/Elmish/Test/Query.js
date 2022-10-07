export const innerText_ = e => e.innerText || ""
export const outerHTML_ = e => e.outerHTML || ""
export const prop_ = (name, e) => e[name] || null // converting `undefined` to `null` so it can be handled via `Nullable`

export const reactComponentName_ = e => {
  const fiberKey = Object.keys(e).find(k => k.startsWith("__reactFiber"))
  const fiber = fiberKey && e[fiberKey]

  if (fiber) {
    // Most nested elements have a Fiber on them, so that's where we get the
    // React component.
    return fiber._debugOwner?.elementType?.displayName || ""
  } else {
    // If no Fiber exists, this must be a root element, in which case the
    // Container will point to the component type.
    const containerKey = Object.keys(e).find(k => k.startsWith("__reactContainer"))
    const container = containerKey && e[containerKey]
    return container?.child?.elementType?.displayName || ""
  }
}
