module Demo.HOC.TopAppBarPage where

import Halogen (ClassName)
import Material.Classes.Typography (mdc_typography)
import Protolude (Aff, Const, Unit, Void, const, unit, ($))

import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type TopAppBarPage =
  { fixedAdjust :: ClassName
  , topAppBar :: HH.ComponentHTML Void () Aff
  }

type Query = Const Void

type Input = Unit

type Message = Void

mkComponent :: TopAppBarPage -> H.Component Query Input Message Aff
mkComponent config =
  H.mkComponent
    { initialState: const unit
    , render: const $ render config
    , eval: H.mkEval H.defaultEval
    }

render :: TopAppBarPage -> HH.ComponentHTML Void () Aff
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
