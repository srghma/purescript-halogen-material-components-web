module Demo.Pages.Elevation where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils (mkComponentStatic)
import Protolude (Maybe(..), ($))

import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Elevation (mdc_elevation____z0, mdc_elevation____z1, mdc_elevation____z10, mdc_elevation____z11, mdc_elevation____z12, mdc_elevation____z13, mdc_elevation____z14, mdc_elevation____z15, mdc_elevation____z16, mdc_elevation____z17, mdc_elevation____z18, mdc_elevation____z19, mdc_elevation____z2, mdc_elevation____z20, mdc_elevation____z21, mdc_elevation____z22, mdc_elevation____z23, mdc_elevation____z24, mdc_elevation____z3, mdc_elevation____z4, mdc_elevation____z5, mdc_elevation____z6, mdc_elevation____z7, mdc_elevation____z8, mdc_elevation____z9)

config :: CatalogPage
config =
    { title: "Elevation"
    , prelude: "Elevation is the relative depth, or distance, between two surfaces along the z-axis."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-elevation"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Elevation"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-elevation"
        }
    , hero: mkComponentStatic $ HH.div_
      [ HH.div [ HP.class_ mdc_elevation____z0, heroSurface ] [ HH.text "Flat 0dp" ]
      , HH.div [ HP.class_ mdc_elevation____z8, heroSurface ] [ HH.text "Raised 8dp" ]
      , HH.div [ HP.class_ mdc_elevation____z16, heroSurface ] [ HH.text "Raised 16dp" ]
      ]
    , content: mkComponentStatic $
        HH.div
          [ HP.style "display: flex; flex-flow: row wrap; justify-content: space-between;"
          ]
          [ HH.div [ HP.class_ mdc_elevation____z0, demoSurface ] [ HH.text "0dp" ]
          , HH.div [ HP.class_ mdc_elevation____z1, demoSurface ] [ HH.text "1dp" ]
          , HH.div [ HP.class_ mdc_elevation____z2, demoSurface ] [ HH.text "2dp" ]
          , HH.div [ HP.class_ mdc_elevation____z3, demoSurface ] [ HH.text "3dp" ]
          , HH.div [ HP.class_ mdc_elevation____z4, demoSurface ] [ HH.text "4dp" ]
          , HH.div [ HP.class_ mdc_elevation____z5, demoSurface ] [ HH.text "5dp" ]
          , HH.div [ HP.class_ mdc_elevation____z6, demoSurface ] [ HH.text "6dp" ]
          , HH.div [ HP.class_ mdc_elevation____z7, demoSurface ] [ HH.text "7dp" ]
          , HH.div [ HP.class_ mdc_elevation____z8, demoSurface ] [ HH.text "8dp" ]
          , HH.div [ HP.class_ mdc_elevation____z9, demoSurface ] [ HH.text "9dp" ]
          , HH.div [ HP.class_ mdc_elevation____z10, demoSurface ] [ HH.text "10dp" ]
          , HH.div [ HP.class_ mdc_elevation____z11, demoSurface ] [ HH.text "11dp" ]
          , HH.div [ HP.class_ mdc_elevation____z12, demoSurface ] [ HH.text "12dp" ]
          , HH.div [ HP.class_ mdc_elevation____z13, demoSurface ] [ HH.text "13dp" ]
          , HH.div [ HP.class_ mdc_elevation____z14, demoSurface ] [ HH.text "14dp" ]
          , HH.div [ HP.class_ mdc_elevation____z15, demoSurface ] [ HH.text "15dp" ]
          , HH.div [ HP.class_ mdc_elevation____z16, demoSurface ] [ HH.text "16dp" ]
          , HH.div [ HP.class_ mdc_elevation____z17, demoSurface ] [ HH.text "17dp" ]
          , HH.div [ HP.class_ mdc_elevation____z18, demoSurface ] [ HH.text "18dp" ]
          , HH.div [ HP.class_ mdc_elevation____z19, demoSurface ] [ HH.text "19dp" ]
          , HH.div [ HP.class_ mdc_elevation____z20, demoSurface ] [ HH.text "20dp" ]
          , HH.div [ HP.class_ mdc_elevation____z21, demoSurface ] [ HH.text "21dp" ]
          , HH.div [ HP.class_ mdc_elevation____z22, demoSurface ] [ HH.text "22dp" ]
          , HH.div [ HP.class_ mdc_elevation____z23, demoSurface ] [ HH.text "23dp" ]
          , HH.div [ HP.class_ mdc_elevation____z24, demoSurface ] [ HH.text "24dp" ]
          ]
    }

heroSurface :: forall r i . IProp (style :: String | r) i
heroSurface = HP.style "display: -ms-inline-flexbox; display: inline-flex; -ms-flex-pack: distribute; justify-content: space-around; min-height: 100px; min-width: 200px; -ms-flex-align: center; align-items: center; width: 120px; height: 48px; margin: 24px; background-color: #212121; color: #f0f0f0;"

demoSurface :: forall r i . IProp (style :: String | r) i
demoSurface = HP.style "display: -ms-inline-flexbox; display: inline-flex; -ms-flex-pack: distribute; justify-content: space-around; min-height: 100px; min-width: 200px; margin: 15px; -ms-flex-align: center; align-items: center;"
