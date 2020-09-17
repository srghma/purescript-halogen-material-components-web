module HalogenMWC.Ripple.Constants where

import Material.Classes.Ripple
import Protolude

import DOM.HTML.Indexed as I
import Data.Int as Int
import Effect.Uncurried as EFn
import FRP.Event (Event) as Event
import Halogen (ClassName(..))
import Halogen (ComponentSlot, ElemName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events (onKeyUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation as Implementation
import HalogenMWC.Button.Insides as Insides
import HalogenMWC.Utils as Utils
import Math as Math
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.Window as Web.HTML.Window
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.Touch as Web.TouchEvent.Touch
import Web.TouchEvent.TouchList as Web.TouchEvent.TouchList
import Web.TouchEvent.TouchEvent as Web.TouchEvent.TouchEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as Web.UIEvent.MouseEvent

cssClasses ::
  { "BG_FOCUSED"      :: ClassName
  , "FG_ACTIVATION"   :: ClassName
  , "FG_DEACTIVATION" :: ClassName
  , "ROOT"            :: ClassName
  , "UNBOUNDED"       :: ClassName
  }
cssClasses =
  { "BG_FOCUSED":      mdc_ripple_upgraded____background_focused
  , "FG_ACTIVATION":   mdc_ripple_upgraded____foreground_activation
  , "FG_DEACTIVATION": mdc_ripple_upgraded____foreground_deactivation
  , "ROOT":            mdc_ripple_upgraded
  , "UNBOUNDED":       mdc_ripple_upgraded____unbounded
  }

strings ::
  { "VAR_FG_SCALE"           :: String
  , "VAR_FG_SIZE"            :: String
  , "VAR_FG_TRANSLATE_END"   :: String
  , "VAR_FG_TRANSLATE_START" :: String
  , "VAR_LEFT"               :: String
  , "VAR_TOP"                :: String
  }
strings =
  { "VAR_FG_SCALE":           "--mdc-ripple-fg-scale"
  , "VAR_FG_SIZE":            "--mdc-ripple-fg-size"
  , "VAR_FG_TRANSLATE_END":   "--mdc-ripple-fg-translate-end"
  , "VAR_FG_TRANSLATE_START": "--mdc-ripple-fg-translate-start"
  , "VAR_LEFT":               "--mdc-ripple-left"
  , "VAR_TOP":                "--mdc-ripple-top"
  }

numbers ::
  { "FG_DEACTIVATION_MS"      :: Int -- Corresponds to $mdc-ripple-fade-out-duration (i.e. deactivation animation duration)
  -- | , "DEACTIVATION_TIMEOUT_MS" :: Int -- Corresponds to $mdc-ripple-translate-duration (i.e. activation animation duration)
  , "INITIAL_ORIGIN_SCALE"    :: Number
  , "PADDING"                 :: Number
  -- | , "TAP_DELAY_MS"            :: Int -- Delay between touch and simulated mouse events on touch devices
  }
numbers =
  { "FG_DEACTIVATION_MS":          1500
  -- | , "DEACTIVATION_TIMEOUT_MS":     225
  , "INITIAL_ORIGIN_SCALE":        0.6
  , "PADDING":                     10.0
  -- | , "TAP_DELAY_MS":                300
  }

pointer_deactivation_event_types :: Array EventType
pointer_deactivation_event_types =
  [ EventType "touchend"
  , EventType "pointerup"
  , EventType "mouseup"
  , EventType "contextmenu"
  ]
