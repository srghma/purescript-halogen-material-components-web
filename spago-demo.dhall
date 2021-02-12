{ name = "demo-app"
, dependencies = (./spago.dhall).dependencies #
  [ "routing-duplex"
  , "argonaut-codecs"
  , "argonaut-generic"
  , "routing"
  ]
, packages = (./spago.dhall).packages
, sources = (./spago.dhall).sources # ["demo/**/*.purs"]
}
