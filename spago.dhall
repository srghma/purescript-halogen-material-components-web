{ name = "halogen-material-components-web"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "numbers"
  , "prelude"
  , "protolude"
  , "psci-support"
  , "record"
  , "strings"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
