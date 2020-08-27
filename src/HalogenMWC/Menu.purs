module HalogenMWC.Menu where

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
import Material.Classes.Menu
import Material.Classes.MenuSurface

type Config i =
  { open :: Boolean
  , quickOpen :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  , additionalClasses :: Array ClassName
  , onClose :: Maybe (Event -> i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { open: false
  , quickOpen: false
  , additionalAttributes: []
  , additionalClasses: []
  , onClose: Nothing
  }

menu :: forall w i . Config i -> Array (HH.HTML w i) -> HH.HTML w i
menu config =
  HH.element (ElemName "mdc-menu")
    ( [ HP.classes $ [mdc_menu, mdc_menu_surface] <> config.additionalClasses
      , HP.prop (PropName "open") config.open
      , HP.prop (PropName "quickOpen") config.quickOpen
      ]
      <> Array.catMaybes
        [ map (HE.handler (EventType "MDCMenu:close")) config.onClose
        ]
      <> config.additionalAttributes
    )
