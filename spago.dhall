{ name = "elmish-testing-library"
, license = "MIT"
, dependencies =
  [ "arrays"
  , "aff"
  , "datetime"
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
, repository = "https://github.com/collegevine/purescript-elmish-testing-library.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
