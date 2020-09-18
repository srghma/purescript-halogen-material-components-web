module Demo.Pages.Ripple where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
-- | import HalogenMWC.Ripple as Ripple
import Material.Classes.Typography (mdc_typography____subtitle1)
import Material.Classes.Elevation (mdc_elevation____z2)
import MaterialIconsFont.Classes (material_icons)
import Demo.Utils (mkComponentStatic)
import Demo.Pages.Ripple.Css as Demo.Pages.Ripple.Css

config :: CatalogPage
config =
    { title: "Ripple"
    , prelude: "Ripples are visual representations used to communicate the status of a component or interactive element."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-states"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Ripple"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-ripple"
        }
    , hero:
        mkComponentStatic $ HH.div_
        [ HH.div demoBox
          [ HH.text "Click here!"
          -- | , Ripple.ripple Ripple.Bounded Ripple.defaultConfig
          ]
        ]
    , content: mkComponentStatic $ HH.div_
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Bounded Ripple" ]
        , HH.div demoBox
            [ HH.text "Interact with me!"
            -- | , Ripple.ripple Ripple.Bounded Ripple.defaultConfig
            ]
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Unbounded Ripple" ]
        , HH.div
            [ HP.class_ material_icons
            , HP.style "width: 24px; height: 24px; padding: 12px; border-radius: 50%; position: relative;"
            ]
            [ HH.text "favorite"
            -- | , Ripple.ripple Ripple.Unbounded Ripple.defaultConfig
            ]
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Theme Color: Primary" ]
        , HH.div demoBox
            [ HH.text "Primary"
            -- | , Ripple.ripple Ripple.Bounded (Ripple.defaultConfig { color = Just Ripple.Primary })
            ]
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Theme Color: Secondary" ]
        , HH.div demoBox
            [ HH.text "Secondary"
            -- | , Ripple.ripple Ripple.Bounded (Ripple.defaultConfig { color = Just Ripple.Accent })
            ]
        ]
    }

demoBox :: forall r i . Array (IProp ( class :: String , style :: String , tabIndex :: Int | r ) i)
demoBox =
    [ HP.classes [mdc_elevation____z2, Demo.Pages.Ripple.Css.styles.demoBox]
    , HP.tabIndex 0
    ]
