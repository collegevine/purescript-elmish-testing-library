{ name = "elmish-testing-library"
, dependencies =
  [ "arrays"
  , "aff"
  , "debug"
  , "effect"
  , "elmish"
  , "elmish-html"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "prelude"
  , "spec"
  , "transformers"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
