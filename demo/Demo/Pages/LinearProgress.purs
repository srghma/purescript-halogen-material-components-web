module Demo.Pages.LinearProgress where

import Demo.HOC.CatalogPage
import Demo.Utils
import Halogen
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Demo.Route (Route(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.LinearProgress (Variant(..), linearProgress)
import HalogenMWC.LinearProgress as LinearProgress

catalogPage :: CatalogPage
catalogPage =
    { title: "Linear Progress Indicator"
    , prelude: "Progress indicators display the length of a process or express an unspecified wait time."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-progress-indicators"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-LinearProgress"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-linear-progress"
        }
    , hero: mkComponentStatic $ HH.div_ [ LinearProgress.linearProgress LinearProgress.defaultConfig (LinearProgress.Determinate 0.5) ]
    , content: mkComponentStatic $ HH.div_
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Buffered" ]
        , LinearProgress.linearProgress LinearProgress.defaultConfig (LinearProgress.Buffered { progress: 0.5, buffer: 0.75 })
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Indeterminate" ]
        , LinearProgress.linearProgress LinearProgress.defaultConfig LinearProgress.Indeterminate
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Reversed" ]
        , LinearProgress.linearProgress (LinearProgress.defaultConfig { reverse = true }) (LinearProgress.Determinate 0.5)
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Reversed Buffered" ]
        , LinearProgress.linearProgress (LinearProgress.defaultConfig { reverse = true }) (LinearProgress.Buffered { progress: 0.5, buffer: 0.75 })
        ]
    }
