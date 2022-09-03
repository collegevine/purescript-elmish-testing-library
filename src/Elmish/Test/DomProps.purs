module Elmish.Test.DomProps where

newtype DomProp (a :: Type) = DomProp String

value = DomProp "value" :: DomProp String

href = DomProp "href" :: DomProp String

disabled = DomProp "disabled" :: DomProp Boolean

checked = DomProp "checked" :: DomProp Boolean
