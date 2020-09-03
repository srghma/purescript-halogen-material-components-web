module Demo.Pages.Chip where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude (Maybe(..))
import Demo.Pages.Chip.Hero as Hero
import Demo.Pages.Chip.Content as Content

config :: CatalogPage
config =
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
