module Demo.Pages.Fab where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude (Maybe(..), const, ($))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import HalogenMWC.Button as Button
import HalogenMWC.Fab as Fab
import HalogenMWC.Fab.Extended as Fab.Extended
import Material.Classes.Typography (mdc_typography____subtitle1)
import Demo.Utils (mkComponentStatic)
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
    , hero: mkComponentStatic $ Fab.fabMateiralIcons Fab.defaultConfig "favorite_border"
    , content:
      let
        render :: forall w. HH.HTML w WithFocus.Action
        render = HH.div_
          [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Standard Floating Action Button" ]
          , Fab.fabMateiralIcons Fab.defaultConfig "favorite_border"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Mini Floating Action Button" ]
          , Fab.fabMateiralIcons (Fab.defaultConfig { mini = true }) "favorite_border"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Extended FAB" ]
          , Fab.Extended.fabMateiralIcons (Fab.Extended.defaultConfig { icon = Just "add" }) "Create"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Extended FAB (Text label followed by icon)" ]
          , Fab.Extended.fabMateiralIcons
              (Fab.Extended.defaultConfig
                  { icon = Just "add"
                  , trailingIcon = true
                  }
              )
              "Create"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Extended FAB (without icon)" ]
          , Fab.Extended.fabMateiralIcons Fab.Extended.defaultConfig "Create"
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "FAB (Shaped)" ]
          , HH.div [ HP.style "display: flex;" ]
              [ Fab.fabMateiralIcons
                  (Fab.defaultConfig
                      { additionalAttributes =
                          [ HP.style "border-radius: 50% 0; margin-right: 24px;"
                          ]
                      }
                  )
                  "favorite_border"
              , Fab.fabMateiralIcons
                  (Fab.defaultConfig
                      { mini = true
                      , additionalAttributes =
                          [ HP.style "border-radius: 8px; margin-right: 24px;"
                          ]
                      }
                  )
                  "favorite_border"
                  , Fab.Extended.fabMateiralIcons (Fab.Extended.defaultConfig { icon = Just "add" }) "Create"
              ]
          , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus FAB" ]
          , HH.div [ HP.style "display: flex;" ]
              [ Fab.fabMateiralIcons (Fab.defaultConfig { additionalAttributes = [ HP.id_ "my-fab" ] }) "favorite_border"
              , HH.text "\x00A0"
              , Button.button Button.Raised
                  (Button.defaultConfig { additionalAttributes = [ HE.onClick $ const $ WithFocus.Focus "my-fab" ] })
                  [ HH.text "Focus" ]
              ]
          ]
      in WithFocus.mkComponent render
    }
