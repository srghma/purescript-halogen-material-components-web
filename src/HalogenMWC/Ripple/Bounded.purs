module HalogenMWC.Ripple.Bounded where

import Protolude

import DOM.HTML.Indexed as I
import Data.Array as Array
import Data.String as String
import FRP.Event as Event
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
import Web.DOM (Element)
import Web.DOM.Document as Web.DOM.Document
import Web.DOM.Element as Web.DOM.Element
import Web.Event.Event (EventType(..))
import Web.Event.Event as Web.Event.Event
import Web.HTML as Web.HTML
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.Window (Window)
import Web.HTML.Window as Web.HTML.Window
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import HalogenMWC.Ripple.Common
import HalogenMWC.Ripple.Calculations
import HalogenMWC.Ripple.Constants
import Halogen.Query.HalogenM as Halogen.Query.HalogenM

handleAction
  :: forall slots output
   . Boolean
  -> H.HalogenM RippleState RippleAction__Common slots output Aff (Maybe Web.HTML.HTMLElement)
  -> RippleAction__Common
  -> H.HalogenM RippleState RippleAction__Common slots output Aff Unit
handleAction isDisabled = handleAction__Common isDisabled false
