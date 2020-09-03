module Demo.HOC.TopAppBarPage where

import Halogen
import Material.Classes.Typography
import Protolude

import DOM.HTML.Indexed (HTMLdiv) as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

type TopAppBarPage w i =
  { fixedAdjust :: ClassName
  , topAppBar :: HH.HTML w i
  }

render :: forall w i . TopAppBarPage w i -> HH.HTML w i
render config =
  HH.div
    [ HP.class_ mdc_typography
    , HP.style "height: 200vh;"
    ]
    [ config.topAppBar
    , HH.div [ HP.class_ config.fixedAdjust ] (Array.replicate 4 (HH.p_ [ HH.text demoParagraph ]))
    ]

demoParagraph :: String
demoParagraph =
  """
  Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
  tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim
  veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea
  commodo consequat.  Duis aute irure dolor in reprehenderit in voluptate
  velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat
  cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id
  est laborum.
  """
