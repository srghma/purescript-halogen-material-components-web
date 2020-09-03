module Demo.Pages.Fab where

import Demo.HOC.CatalogPage (CatalogPage)
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
import HalogenMWC.Button as Button
import HalogenMWC.Fab as Fab
import HalogenMWC.Fab.Extended as Fab.Extended
import Material.Classes.Typography
import Demo.Utils
import Demo.MkComponent.WithFocus as WithFocus

config :: CatalogPage
config =
    { title: "Floating Action Button"
    , prelude: "Floating action buttons represents the primary action in an application. Only one floating action button is recommended per screen to represent the most common action."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-fab"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Fab"
        , sourceCode: Just "https://github.com/material-components/material-components-web/blob/master/packages/mdc-fab/"
        }
    , hero: mkComponentStatic $ Fab.fab Fab.defaultConfig "favorite_border"
    , content:
      let
        render :: forall w. HH.HTML w WithFocus.Action
        render = HH.div_
          [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Standard Floating Action Button" ]
          , Fab.fab Fab.defaultConfig "favorite_border"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Mini Floating Action Button" ]
          , Fab.fab (Fab.defaultConfig { mini = true }) "favorite_border"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Extended FAB" ]
          , Fab.Extended.fab (Fab.Extended.defaultConfig { icon = Just "add" }) "Create"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Extended FAB (Text label followed by icon)" ]
          , Fab.Extended.fab
              (Fab.Extended.defaultConfig
                  { icon = Just "add"
                  , trailingIcon = true
                  }
              )
              "Create"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Extended FAB (without icon)" ]
          , Fab.Extended.fab Fab.Extended.defaultConfig "Create"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "FAB (Shaped)" ]
          , HH.div [ HP.style "display: flex;" ]
              [ Fab.fab
                  (Fab.defaultConfig
                      { additionalAttributes =
                          [ HP.style "border-radius: 50% 0; margin-right: 24px;"
                          ]
                      }
                  )
                  "favorite_border"
              , Fab.fab
                  (Fab.defaultConfig
                      { mini = true
                      , additionalAttributes =
                          [ HP.style "border-radius: 8px; margin-right: 24px;"
                          ]
                      }
                  )
                  "favorite_border"
                  , Fab.Extended.fab (Fab.Extended.defaultConfig { icon = Just "add" }) "Create"
              ]
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus FAB" ]
          , HH.div [ HP.style "display: flex;" ]
              [ Fab.fab (Fab.defaultConfig { additionalAttributes = [ HP.id_ "my-fab" ] }) "favorite_border"
              , HH.text "\x00A0"
              , Button.button Button.Raised
                  (Button.defaultConfig { additionalAttributes = [ HE.onClick $ const $ WithFocus.Focus "my-fab" ] })
                  [ HH.text "Focus" ]
              ]
          ]
      in WithFocus.mkComponent render
    }
