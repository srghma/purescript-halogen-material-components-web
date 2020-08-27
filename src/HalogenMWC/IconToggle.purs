module HalogenMWC.IconToggle where

import Material.Classes.IconButton
import MaterialIconsFont.Classes
import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Material.Classes.IconButton

type Config i =
  { on :: Boolean
  , disabled :: Boolean
  , label :: Maybe String
  , additionalAttributes :: Array (IProp I.HTMLbutton i)
  , onChange :: Maybe (Event -> i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { on: false
  , disabled: false
  , label: Nothing
  , additionalAttributes: []
  , onChange: Nothing
  }

iconToggle :: forall w i . Config i -> { onIcon :: String, offIcon :: String } -> HH.HTML w i
iconToggle config { onIcon, offIcon } =
  HH.element (ElemName "mdc-icon-button")
    ( [ HP.class_ mdc_icon_button
      , HP.prop (PropName "on") config.on
      , HP.tabIndex 0
      , HP.attr (AttrName "aria-hidden") "true"
      , HP.attr (AttrName "aria-pressed") (if config.on then "true" else "false")
      , HP.disabled config.disabled
      ]
      <> Array.catMaybes
      [ map (HP.attr (AttrName "aria-label")) config.label
      , map (HE.handler (EventType "MDCIconButtonToggle:change")) config.onChange
      ]
      <> config.additionalAttributes
    )
    [ HH.i [ HP.classes [ material_icons, mdc_icon_button__icon, mdc_icon_button__icon____on ] ] [ HH.text onIcon ]
    , HH.i [ HP.classes [ material_icons, mdc_icon_button__icon ] ] [ HH.text offIcon ]
    ]
