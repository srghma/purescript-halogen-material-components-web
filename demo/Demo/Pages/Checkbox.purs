module Demo.Pages.Checkbox where

import Demo.HOC.CatalogPage
import Data.Map (Map)
import Data.Map as Map
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
import HalogenMWC.Checkbox as Checkbox
import Material.Classes.Typography
import Demo.Pages.Checkbox.Content as Content
import Demo.Pages.Checkbox.Hero as Hero

catalogPage :: CatalogPage
catalogPage =
  { title: "Checkbox"
  , prelude: "Checkboxes allow the user to select multiple options from a set."
  , resources:
      { materialDesignGuidelines: Just "https://material.io/go/design-checkboxes"
      , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Checkbox"
      , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-checkbox"
      }
  , hero: Hero.component
  , content: Content.component
  }
