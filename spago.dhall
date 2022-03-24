{ name = "decimals"
, dependencies =
  [ "assert"
  , "console"
  , "effect"
  , "integers"
  , "math"
  , "maybe"
  , "numbers"
  , "prelude"
  , "psci-support"
  , "quickcheck"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
