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
  ----
  , "protolude"
  , "routing-duplex"
  , "generics-rep"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "routing"
  , "ordered-collections"
  , "profunctor-lenses"
  ]
, packages = ./packages.dhall
, sources =
  [ "src/**/*.purs"
  , "test/**/*.purs"
  , "demo/**/*.purs"
  ]
}
