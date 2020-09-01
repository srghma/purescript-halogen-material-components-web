module Demo.Pages.Drawer where

import Demo.HOC.CatalogPage
import Demo.Utils
import Halogen
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Drawer.Permanent as Drawer.Permanent
import HalogenMWC.Icon as Icon
import HalogenMWC.List as List
import HalogenMWC.List.Item as List.Item
import Material.Classes.Drawer
-- | import Material.Classes.List

catalogPage :: CatalogPage
catalogPage =
  { title: "Drawer"
  , prelude: "The navigation drawer slides in from the left and contains the navigation destinations for your app."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-navigation-drawer"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Drawer"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-drawer"
      }
  , hero: mkComponentStatic $ HH.div_ heroDrawer
  , content: mkComponentStatic $ HH.div_
      [ iframe "Permanent" "#permanent-drawer"
      , iframe "Dismissible" "#dismissible-drawer"
      , iframe "Modal" "#modal-drawer"
      ]
  }

heroDrawer :: forall r w i . Array (HH.HTML w i)
heroDrawer =
  let
    listItem ( activated /\ icon /\ label ) =
      List.Item.listItem
        (List.Item.defaultConfig
          { selected = if activated then Just List.Item.Activated else Nothing
          , additionalAttributes = [ HP.href "#drawer" ]
          }
        )
        [ HH.div [ HP.class_ mdc_list_item__graphic ] [ Icon.materialIcon [] icon ]
        , HH.text label
        ]
  in
  [ Drawer.Permanent.drawer
    [ Drawer.Permanent.header
      [ HH.h3 [ HP.class_ mdc_drawer__title ] [ HH.text "Title" ]
      , HH.h6 [ HP.class_ mdc_drawer__subtitle ] [ HH.text "Subtitle" ]
      ]
    , Drawer.Permanent.content
      [ List.list List.defaultConfig
        (listItem ( true /\ "inbox" /\ "Inbox" ))
        (map listItem
          [ ( false /\ "star" /\ "Star" )
          , ( false /\ "send" /\ "Sent Mail" )
          , ( false /\ "drafts" /\ "Drafts" )
          ]
        )
      ]
    ]
  ]

iframe :: forall r w i . String -> String -> HH.HTML w i
iframe label route =
  HH.div
    [ HP.style "display: inline-block; -ms-flex: 1 1 80%; flex: 1 1 80%; -ms-flex-pack: distribute; justify-content: space-around; min-height: 400px; min-width: 400px; padding: 15px;"
    ]
    [ HH.div_
        [ HH.a
            [ HP.href route
            , HP.target "_blank"
            ]
            [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text label ] ]
        ]
    , HH.iframe
        [ HP.src route
        , HP.style "height: 400px; width: 100vw; max-width: 780px;"
        ]
    ]
