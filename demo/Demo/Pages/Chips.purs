module Demo.Pages.Chips where

import Data.Set (Set)
import Data.Set as Set
import Demo.HOC.CatalogPage
import Halogen
import Halogen
import Material.Classes.Typography
import Protolude
import Protolude
import Data.Array as Array
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Maybe as Maybe
import Halogen as H
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.Chip.Action as Chip.Action
import HalogenMWC.Chip.Choice as Chip.Choice
import HalogenMWC.Chip.Filter as Chip.Filter
import HalogenMWC.Chip.Input as Chip.Input
import HalogenMWC.ChipSet.Action as ChipSet.Action
import HalogenMWC.ChipSet.Choice as ChipSet.Choice
import HalogenMWC.ChipSet.Filter as ChipSet.Filter
import HalogenMWC.ChipSet.Input as ChipSet.Input
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Data.String as String
import Demo.Utils
import Demo.Pages.Chips.Hero as Hero
import Demo.Pages.Chips.Content as Content

catalogPage :: CatalogPage
catalogPage =
  { title: "Chips"
  , prelude: "Chips are compact elements that allow users to enter information, select a choice, filter content, or trigger an action."
  , resources:
    { materialDesignGuidelines: Just "https://material.io/go/design-chips"
    , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Chips"
    , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips"
    }
  , hero: Hero.component
  , content: Content.component
  }
