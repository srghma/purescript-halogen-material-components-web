module Demo.Blocks.Header where

import Protolude (Const, Maybe(..), Unit, Void, const, map, unit)
import Material.Classes.TopAppBar (mdc_top_app_bar__navigation_icon, mdc_top_app_bar__row, mdc_top_app_bar__section, mdc_top_app_bar__section____align_start, mdc_top_app_bar__title)
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
