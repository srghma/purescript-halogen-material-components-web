module Demo.Pages.Select where

import Demo.HOC.CatalogPage (CatalogPage)
import Protolude
import Demo.Pages.Select.Hero as Hero
import Demo.Pages.Select.Content as Content

config :: CatalogPage
config =
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
