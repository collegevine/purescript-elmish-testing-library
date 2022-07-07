{ name = "elmish-testing-library"
, dependencies =
  [ "arrays"
  , "aff"
  , "debug"
  , "effect"
  , "elmish"
  , "elmish-html"
  , "exceptions"
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
