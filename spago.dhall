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
  , "psci-support"
  , "record"
  , "strings"
  , "tuples"
  , "either"
  , "foldable-traversable"
  , "js-timers"
  , "protolude"
  , "profunctor-lenses"
  , "ordered-collections"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  ]
}
