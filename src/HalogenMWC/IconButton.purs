module HalogenMWC.IconButton where

import Halogen
import Material.Classes.IconButton
import MaterialIconsFont.Classes
import Protolude
import Web.Event.Event

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Web.UIEvent.MouseEvent (MouseEvent)

type Config i =
  { disabled :: Boolean
  , label :: Maybe String
  , additionalClasses :: Array ClassName
  , additionalAttributes :: Array (IProp I.HTMLbutton i)
  , onClick :: Maybe (MouseEvent -> i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { disabled: false
  , label: Nothing
  , additionalClasses: []
  , additionalAttributes: []
  , onClick: Nothing
  }

iconButton :: forall w i . Config i -> String -> HH.HTML w i
iconButton config iconName =
  HH.element (ElemName "mdc-icon-button")
    ( [ HP.classes $ [ mdc_icon_button, material_icons ] <> config.additionalClasses
      , HP.tabIndex 0
      ]
      <> case config.onClick of
              Nothing -> []
              Just onClick -> [ HE.onClick onClick ]
      <> config.additionalAttributes
    )
    [ HH.text iconName ]
