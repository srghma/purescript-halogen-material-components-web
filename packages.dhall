let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200724/packages.dhall sha256:bb941d30820a49345a0e88937094d2b9983d939c9fd3a46969b85ce44953d7d9

let overrides =
      { halogen =
              upstream.halogen
          //  { repo = "https://github.com/srghma/purescript-halogen.git"
              , version = "hydration-wip-2"
              }
      }

let additions =
      { protolude =
        { dependencies =
          [ "affjax"
          , "console"
          , "effect"
          , "node-fs-aff"
          , "node-process"
          , "node-path"
          , "prelude"
          , "proxy"
          , "psci-support"
          , "record"
          , "typelevel-prelude"
          , "debug"
          , "variant"
          , "ansi"
          , "generics-rep"
          ]
        , repo = "ssh://git@github.com/srghma/purescript-protolude.git"
        , version = "master"
        }
      , either =
        { dependencies =
          [ "bifunctors"
          , "control"
          , "foldable-traversable"
          , "invariant"
          , "maybe"
          , "prelude"
          ]
        , repo = "ssh://git@github.com/srghma/purescript-either.git"
        , version = "patch-1"
        }
      , halogen-vdom =
        { dependencies =
          [ "bifunctors"
          , "console"
          , "effect"
          , "exists"
          , "foreign"
          , "foreign-object"
          , "js-timers"
          , "maybe"
          , "prelude"
          , "psci-support"
          , "refs"
          , "tuples"
          , "unsafe-coerce"
          , "web-html"
          , "web-dom"
          , "debug"
          , "strings"
          , "control"
          , "lazy"
          ]
        , repo = "ssh://git@github.com/srghma/purescript-halogen-vdom.git"
        , version = "master"
        }
      , halogen-svg =
        { dependencies = [ "prelude" ]
        , repo = "ssh://git@github.com/srghma/purescript-halogen-svg.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
