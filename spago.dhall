{ name = "decimals"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "integers"
  , "maybe"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
