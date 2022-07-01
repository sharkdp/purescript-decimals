{ name = "decimals"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "quickcheck"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
