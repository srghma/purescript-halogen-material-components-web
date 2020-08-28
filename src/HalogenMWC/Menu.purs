module HalogenMWC.Menu where

import Prelude
import DOM.HTML.Indexed as I
import Web.Event.Event (Event, EventType(..))
import Data.Array as Array
import Halogen (ClassName, ElemName(..), PropName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Material.Classes.Menu (mdc_menu)
import Material.Classes.MenuSurface (mdc_menu_surface)

type Config i
  = { open :: Boolean
    , quickOpen :: Boolean
    , additionalAttributes :: Array (IProp I.HTMLdiv i)
    , additionalClasses :: Array ClassName
    , onClose :: Maybe (Event -> i)
    }

defaultConfig :: forall i. Config i
defaultConfig =
  { open: false
  , quickOpen: false
  , additionalAttributes: []
  , additionalClasses: []
  , onClose: Nothing
  }

menu :: forall w i. Config i -> Array (HH.HTML w i) -> HH.HTML w i
menu config =
  HH.element (ElemName "mdc-menu")
    ( [ HP.classes $ [ mdc_menu, mdc_menu_surface ] <> config.additionalClasses
      , HP.prop (PropName "open") config.open
      , HP.prop (PropName "quickOpen") config.quickOpen
      ]
        <> Array.catMaybes
            [ map (HE.handler (EventType "MDCMenu:close")) config.onClose
            ]
        <> config.additionalAttributes
    )
