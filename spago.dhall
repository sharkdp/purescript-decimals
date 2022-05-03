{ name = "decimals"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
