module Demo.Pages.Selects where

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
import HalogenMWC.Select as Select
import HalogenMWC.Select.Item as Select.Item
import Material.Classes.Typography
import Demo.Utils
import Demo.Pages.Selects.Hero as Hero
import Demo.Pages.Selects.Content as Content

catalogPage :: CatalogPage
catalogPage =
    { title: "Select"
    , prelude: "Selects allow users to select from a single-option menu."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-text-fields"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Select"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-select"
        }
    , hero: Hero.component
    , content: Content.component
    }
