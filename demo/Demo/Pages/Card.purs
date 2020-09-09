module Demo.Pages.Card where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Material.Classes.Theme (mdc_theme____text_secondary_on_background)
import Material.Classes.Typography (mdc_typography____body2, mdc_typography____headline6, mdc_typography____subtitle2)
import Protolude (Const, Maybe(..), Unit, Void, const, ($))
import Data.Array as Array
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button as Button
import HalogenMWC.Card as Card
import HalogenMWC.IconButton as IconButton
import Demo.MkComponent.WithFocus as WithFocus

type State
  = Unit

type Query
  = Const Void

type Input
  = Unit

type Message
  = Void

config :: CatalogPage
config =
  { title: "Card"
  , prelude: "Cards contain content and actions about a single subject."
  , resources:
    { materialDesignGuidelines: Just "https://material.io/go/design-cards"
    , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Card"
    , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-card"
    }
  , hero: mkComponentStatic heroCard
  , content:
    WithFocus.mkComponent
      $ HH.div_
          [ exampleCard1
          , exampleCard2
          , exampleCard3
          , focusCard
          ]
  }

heroCard :: forall w i. HH.HTML w i
heroCard =
  Card.card
    ( Card.defaultConfig
        { additionalAttributes = [ HP.style "width: 350px;" ]
        }
    )
    { blocks:
      Array.singleton
        $ Card.primaryAction []
            [ demoMedia
            , demoTitle
            , demoBody
            ]
    , actions: Just demoActions
    }

exampleCard1 :: forall w i. HH.HTML w i
exampleCard1 =
  Card.card
    ( Card.defaultConfig
        { additionalAttributes = [ HP.style "margin: 48px 0; width: 350px;" ]
        }
    )
    { blocks:
      Array.singleton
        $ Card.primaryAction []
            [ demoMedia
            , demoTitle
            , demoBody
            ]
    , actions: Nothing
    }

exampleCard2 :: forall w i. HH.HTML w i
exampleCard2 =
  Card.card
    ( Card.defaultConfig
        { additionalAttributes =
          [ HP.style "margin: 48px 0; width: 350px;"
          ]
        }
    )
    { blocks:
      Array.singleton
        $ Card.primaryAction []
            [ demoTitle
            , demoBody
            ]
    , actions: Just demoActions
    }

exampleCard3 :: forall w i. HH.HTML w i
exampleCard3 =
  Card.card
    ( Card.defaultConfig
        { additionalAttributes =
          [ HP.style "margin: 48px 0; width: 350px; border-radius: 24px 8px;"
          ]
        }
    )
    { blocks:
      Array.singleton
        $ Card.primaryAction []
            [ demoTitle
            , demoBody
            ]
    , actions: Just demoActions
    }

focusCard :: forall r w i. HH.HTML w WithFocus.Action
focusCard =
  HH.div_
    [ Card.card
        ( Card.defaultConfig
            { additionalAttributes =
              [ HP.id_ "my-card"
              , HP.style "margin: 48px 0; width: 350px;"
              ]
            }
        )
        { blocks:
          Array.singleton
            $ Card.primaryAction []
                [ demoTitle
                , demoBody
                ]
        , actions: Just demoActions
        }
    , HH.text "\x00A0"
    , Button.buttonView Button.Raised
        (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ WithFocus.Focus "my-card") ] })
        [ HH.text "Focus" ]
    ]

demoMedia :: forall w i. HH.HTML w i
demoMedia = Card.mediaView (Just Card.SixteenToNine) [] "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/2.jpg"

demoTitle :: forall w i. HH.HTML w i
demoTitle =
  HH.div
    [ HP.style "padding: 1rem;" ]
    [ HH.h2
        [ HP.class_ mdc_typography____headline6
        , HP.style "margin: 0;"
        ]
        [ HH.text "Our Changing Planet" ]
    , HH.h3
        [ HP.classes [ mdc_typography____subtitle2, mdc_theme____text_secondary_on_background ]
        , HP.style "margin: 0;"
        ]
        [ HH.text "by Kurt Wagner" ]
    ]

demoBody :: forall w i. HH.HTML w i
demoBody =
  HH.div
    [ HP.classes [ mdc_typography____body2, mdc_theme____text_secondary_on_background ]
    , HP.style "padding: 0 1rem 0.5rem 1rem;"
    ]
    [ HH.text
        """
      Visit ten places on our planet that are undergoing the biggest
      changes today.
      """
    ]

demoActions :: forall r w i. Card.Actions w i
demoActions =
  { buttons:
    [ Card.button Button.defaultConfig [ HH.text "Read" ]
    , Card.button Button.defaultConfig [ HH.text "Bookmark" ]
    ]
  , icons:
    [ Card.iconMaterialIcons IconButton.defaultConfig "favorite_border"
    , Card.iconMaterialIcons IconButton.defaultConfig "share"
    , Card.iconMaterialIcons IconButton.defaultConfig "more_vert"
    ]
  , fullBleed: false
  }
