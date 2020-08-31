module HalogenMWC.IconButton where

import Halogen (ClassName, ElemName(..))
import Material.Classes.IconButton (mdc_icon_button)
import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Config i
  = { disabled :: Boolean
    , label :: Maybe String
    , additionalClasses :: Array ClassName
    , additionalAttributes :: Array (IProp I.HTMLbutton i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { disabled: false
  , label: Nothing
  , additionalClasses: []
  , additionalAttributes: []
  }

iconButton :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
iconButton config =
  HH.element (ElemName "mdc-icon-button")
    ( [ HP.classes $ [ mdc_icon_button ] <> config.additionalClasses
      , HP.tabIndex 0
      ]
      <> config.additionalAttributes
    )
