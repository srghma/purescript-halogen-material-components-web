module Demo.Pages.TopAppBar where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Route
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.IconButton as IconButton
import HalogenMWC.TopAppBar as TopAppBar
import Material.Classes.Typography
import Demo.Utils

catalogPage :: CatalogPage
catalogPage =
    { title: "Top App Bar"
    , prelude: "Top App Bars are a container for items such as application title, navigation icon, and action items."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-app-bar-top"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-TopAppBar"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-top-app-bar"
        }
    , hero:
        [ HH.div
            [ HP.style "width: 480px; height: 72px;"
            ]
            [ TopAppBar.topAppBar TopAppBar.Regular
              (TopAppBar.defaultConfig { additionalAttributes = [ HP.style "position: static;" ] })
              [ HH.section [ HP.classes [ mdc_top_app_bar__section ] ]
                  [ HP.class_ mdc_top_app_bar__section____align_start ]
                  [ IconButton.iconButtonMaterialIcons
                      (IconButton.defaultConfig
                          { additionalClasses = [ mdc_top_app_bar__navigation_icon ]
                          }
                      )
                      "menu"
                  , HH.span [ HP.class_ mdc_top_app_bar__title ] [ HH.text "Title" ]
                  ]
              , HH.section [ HP.classes [ mdc_top_app_bar__section ] ]
                  [ HP.class_ mdc_top_app_bar__section____align_end ]
                  [ IconButton.iconButtonMaterialIcons
                      (IconButton.defaultConfig
                          { additionalClasses = [ mdc_top_app_bar__action_item ]
                          }
                      )
                      "file_download"
                  , IconButton.iconButtonMaterialIcons
                      (IconButton.defaultConfig
                          { additionalClasses = [ mdc_top_app_bar__action_item ]
                          }
                      )
                      "print"
                  , IconButton.iconButtonMaterialIcons
                      (IconButton.defaultConfig
                          { additionalClasses = [ mdc_top_app_bar__action_item ]
                          }
                      )
                      "more_vert"
                  ]
              ]
            ]
        ]
    , content:
        [ HH.div
            [ HP.style "display: -ms-flexbox; display: flex; -ms-flex-wrap: wrap; flex-wrap: wrap; min-height: 200px;"
            ]
            [ iframe "Standard" Route.StandardTopAppBar
            , iframe "Fixed" Route.FixedTopAppBar
            , iframe "Dense" Route.DenseTopAppBar
            , iframe "Prominent" Route.ProminentTopAppBar
            , iframe "Short" Route.ShortTopAppBar
            , iframe "Short - Always Collapsed" Route.ShortCollapsedTopAppBar
            ]
        ]
    }

iframe :: forall r w i . String -> Url -> HH.HTML w i
iframe title route =
    let
      stringUrl = Route.toString route
    in
    HH.div
        [ HP.style "display: inline-block; -ms-flex: 1 1 45%; flex: 1 1 45%; -ms-flex-pack: distribute; justify-content: space-around; min-height: 200px; min-width: 400px; padding: 15px;"
        ]
        [ HH.div_
            [ HH.a
                [ HP.href stringUrl
                , HP.target "_blank"
                ]
                [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text title ]
                ]
            ]
        , HH.iframe
            [ HP.style "width: 100%; height: 200px;"
            , HP.src stringUrl
            ]
            []
        ]
