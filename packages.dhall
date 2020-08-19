let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200724/packages.dhall sha256:bb941d30820a49345a0e88937094d2b9983d939c9fd3a46969b85ce44953d7d9

let overrides =
      { halogen =
              upstream.halogen
          //  { repo = "https://github.com/srghma/purescript-halogen.git"
              , version = "hydration-wip-2"
              }
      , halogen-hooks =
              upstream.halogen-hooks
          //  { repo = "https://github.com/srghma/purescript-halogen-hooks.git"
              , version = "master"
              }
      , halogen-storybook =
              upstream.halogen-storybook
          //  { repo =
                  "https://github.com/srghma/purescript-halogen-storybook.git"
              , version = "master"
              }
      , halogen-select =
              upstream.halogen-select
          //  { repo = "https://github.com/srghma/purescript-halogen-select.git"
              , version = "master"
              }
      , halogen-formless =
              upstream.halogen-formless
          //  { repo =
                  "https://github.com/srghma/purescript-halogen-formless.git"
              , version = "master"
              }
      , node-http =
              upstream.node-http
          //  { repo = "https://github.com/srghma/purescript-node-http.git"
              , version = "master"
              }
      , hyper =
              upstream.hyper
          //  { repo = "https://github.com/srghma/hyper.git"
              , version = "patch-1"
              }
      , web-dom =
              upstream.web-dom
          //  { repo = "https://github.com/srghma/purescript-web-dom.git"
              , version = "patch-1"
              }
      , media-types =
              upstream.media-types
          //  { repo = "https://github.com/srghma/purescript-media-types.git"
              , version = "patch-1"
              }
      , dom-indexed =
              upstream.dom-indexed
          //  { repo = "https://github.com/srghma/purescript-dom-indexed.git"
              , version = "patch-1"
              }
      , argonaut-core =
              upstream.argonaut-core
          //  { repo = "https://github.com/srghma/purescript-argonaut-core.git"
              , version = "master"
              }
      , argonaut-codecs =
              upstream.argonaut-codecs
          //  { repo =
                  "https://github.com/purescript-contrib/purescript-argonaut-codecs.git"
              , version = "master"
              }
      , argonaut-generic =
              upstream.argonaut-generic
          //  { repo =
                  "https://github.com/srghma/purescript-argonaut-generic.git"
              , version = "master"
              }
      , argonaut-traversals =
              upstream.argonaut-traversals
          //  { repo =
                  "https://github.com/srghma/purescript-argonaut-traversals.git"
              , version = "master"
              }
      , argonaut =
              upstream.argonaut
          //  { repo = "https://github.com/srghma/purescript-argonaut.git"
              , version = "patch-1"
              }
      , slug =
              upstream.slug
          //  { repo = "https://github.com/srghma/purescript-slug.git"
              , version = "master"
              }
      }

let additions =
      { codec =
        { dependencies = [ "profunctor", "transformers" ]
        , repo = "ssh://git@github.com/garyb/purescript-codec.git"
        , version = "master"
        }
      , halogen-svg =
        { dependencies = [ "prelude" ]
        , repo = "ssh://git@github.com/srghma/purescript-halogen-svg.git"
        , version = "master"
        }
      , halogen-vdom-string-renderer =
        { dependencies =
          [ "prelude", "halogen-vdom", "ordered-collections", "foreign" ]
        , repo =
            "ssh://git@github.com/srghma/purescript-halogen-vdom-string-renderer.git"
        , version = "master"
        }
      , ace =
        { dependencies =
          [ "effect"
          , "web-html"
          , "web-uievents"
          , "arrays"
          , "foreign"
          , "nullable"
          , "prelude"
          ]
        , repo = "ssh://git@github.com/purescript-contrib/purescript-ace.git"
        , version = "master"
        }
      , yarn =
        { dependencies =
          [ "strings", "arrays", "generics-rep", "partial", "unicode" ]
        , repo = "ssh://git@github.com/Thimoteus/purescript-yarn.git"
        , version = "master"
        }
      , protolude =
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
      , web-intersection-observer =
        { dependencies = [ "prelude", "web-dom" ]
        , repo =
            "ssh://git@github.com/srghma/purescript-web-intersection-observer.git"
        , version = "master"
        }
      }

in  upstream // overrides // additions
