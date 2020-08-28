module HalogenMWC.IconButton where

import Halogen (ClassName, ElemName(..))
import Material.Classes.IconButton (mdc_icon_button)
import MaterialIconsFont.Classes (material_icons)
import Prelude
import Data.Maybe (Maybe(..))
import DOM.HTML.Indexed as I
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i
  = { disabled :: Boolean
    , label :: Maybe String
    , additionalClasses :: Array ClassName
    , additionalAttributes :: Array (IProp I.HTMLbutton i)
    , onClick :: Maybe (MouseEvent -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { disabled: false
  , label: Nothing
  , additionalClasses: []
  , additionalAttributes: []
  , onClick: Nothing
  }

iconButton :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
iconButton config =
  HH.element (ElemName "mdc-icon-button")
    ( [ HP.classes $ [ mdc_icon_button ] <> config.additionalClasses
      , HP.tabIndex 0
      ]
      <> case config.onClick of
          Nothing -> []
          Just onClick -> [ HE.onClick onClick ]
      <> config.additionalAttributes
    )
