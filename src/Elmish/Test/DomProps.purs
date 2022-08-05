module Elmish.Test.DomProps where

newtype DomProp (a :: Type) = DomProp String

value = DomProp "value" :: DomProp String

disabled = DomProp "disabled" :: DomProp Boolean
