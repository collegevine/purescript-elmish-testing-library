{ name = "elmish-testing-library"
, dependencies =
  [ "arrays"
  , "console"
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
