module Demo.Pages.Typography where

import Demo.Utils (mkComponentStatic)
import Protolude (Maybe(..), ($))

import Demo.HOC.CatalogPage (CatalogPage)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Material.Classes.Typography (mdc_typography____body1, mdc_typography____body2, mdc_typography____button, mdc_typography____caption, mdc_typography____headline1, mdc_typography____headline2, mdc_typography____headline3, mdc_typography____headline4, mdc_typography____headline5, mdc_typography____headline6, mdc_typography____overline, mdc_typography____subtitle1, mdc_typography____subtitle2)
config :: CatalogPage
config =
    { title: "Typography"
    , prelude: "Roboto is the standard typeface on Android and Chrome."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-typography"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-Typography"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-typography"
        }
    , hero: mkComponentStatic $ HH.h1 [ HP.class_ mdc_typography____headline1 ] [ HH.text "Typography" ]
    , content: mkComponentStatic $ HH.div_
        [ HH.h1 [ HP.class_ mdc_typography____headline1 ] [ HH.text "Headline 1" ]
        , HH.h2 [ HP.class_ mdc_typography____headline2 ] [ HH.text "Headline 2" ]
        , HH.h3 [ HP.class_ mdc_typography____headline3 ] [ HH.text "Headline 3" ]
        , HH.h4 [ HP.class_ mdc_typography____headline4 ] [ HH.text "Headline 4" ]
        , HH.h5 [ HP.class_ mdc_typography____headline5 ] [ HH.text "Headline 5" ]
        , HH.h6 [ HP.class_ mdc_typography____headline6 ] [ HH.text "Headline 6" ]
        , HH.h6 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Subtitle 1" ]
        , HH.h6 [ HP.class_ mdc_typography____subtitle2 ] [ HH.text "Subtitle 2" ]
        , HH.p [ HP.class_ mdc_typography____body1 ] [ HH.text body1Paragraph ]
        , HH.p [ HP.class_ mdc_typography____body2 ] [ HH.text body2Paragraph ]
        , HH.div [ HP.class_ mdc_typography____button ] [ HH.text "Button.button Button.Text" ]
        , HH.div [ HP.class_ mdc_typography____caption ] [ HH.text "Caption text" ]
        , HH.div [ HP.class_ mdc_typography____overline ] [ HH.text "Overline text" ]
        ]
    }

body1Paragraph :: String
body1Paragraph =
    "Body 1. Lorem ipsum dolor sit amet, consectetur adipisicing elit. Quos blanditiis tenetur unde suscipit, quam beatae rerum inventore consectetur, neque doloribus, cupiditate numquam dignissimos laborum fugiat deleniti? Eum quasi quidem quibusdam."

body2Paragraph :: String
body2Paragraph =
    "Body 2. Lorem ipsum dolor sit amet consectetur adipisicing elit. Cupiditate aliquid ad quas sunt voluptatum officia dolorum cumque, possimus nihil molestias sapiente necessitatibus dolor saepe inventore, soluta id accusantium voluptas beatae."
