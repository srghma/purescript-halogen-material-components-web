module Demo.Pages.Chips where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude (Maybe(..))
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
