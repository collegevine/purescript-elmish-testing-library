module Elmish.Test.DomProps where

class DomPropType a where defaultValue :: a
instance DomPropType String where defaultValue = ""
instance DomPropType Boolean where defaultValue = false

newtype DomProp (a :: Type) = DomProp String

value = DomProp "value" :: DomProp String

disabled = DomProp "disabled" :: DomProp Boolean

checked = DomProp "checked" :: DomProp Boolean
