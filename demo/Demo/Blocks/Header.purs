module Demo.Blocks.Header where

import Material.Classes.TopAppBar (mdc_top_app_bar__title)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

header :: forall w i . HH.HTML w i
header = HH.a
  [ HP.class_ mdc_top_app_bar__title
  , HP.style "text-transform: uppercase; font-weight: 400; color: white;"
  , HP.href "https://github.com/srghma/purescript-halogen-material-components-web"
  , HP.target "_blank"
  ]
  [ HH.text "Material Components for Halogen" ]
