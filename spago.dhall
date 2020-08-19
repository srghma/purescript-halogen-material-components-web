{ name = "halogen-material-components-web"
, dependencies =
  [ "arrays"
  , "console"
  , "debug"
  , "event"
  , "halogen-svg"
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
