{ name = "elmish-testing-library"
, license = "MIT"
, dependencies =
  [ "arrays"
  , "aff"
  , "datetime"
  , "effect"
  , "elmish"
  , "exceptions"
  , "foldable-traversable"
  , "maybe"
  , "nullable"
  , "prelude"
  , "transformers"
  , "web-dom"
  , "web-html"
  ]
, packages = ./packages.dhall
, repository = "https://github.com/collegevine/purescript-elmish-testing-library.git"
, sources = [ "src/**/*.purs" ]
}
