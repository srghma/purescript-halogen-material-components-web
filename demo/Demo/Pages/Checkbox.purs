module Demo.Pages.Checkbox where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude (Maybe(..))
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
