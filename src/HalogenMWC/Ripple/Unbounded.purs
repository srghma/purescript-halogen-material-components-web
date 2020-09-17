module HalogenMWC.Ripple.Unbounded where

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

data RippleAction__Unbounded
  = Initialize
  | WindowResized
  | RippleAction__Common RippleAction__Common

-- only for unbounded
handleAction__Initialize
  :: forall output slots
   . H.HalogenM RippleState RippleAction__Unbounded slots output Aff (Maybe Web.HTML.HTMLElement)
  -> H.HalogenM RippleState RippleAction__Unbounded slots output Aff Unit
handleAction__Initialize getRootElementRef = do
  -- register WindowResized
  window <- H.liftEffect Web.HTML.window

  let event :: Event.Event RippleAction__Unbounded
      event =
        (Utils.eventListenerEventSourceWithOptions (EventType "resize") Utils.unsafePassiveIfSupportsAddEventListenerOptions (Web.HTML.Window.toEventTarget window))
        <#> const WindowResized

  void $ H.subscribe event

  -- Unbounded ripples need layout logic applied immediately to set coordinates for both shade and ripple
  layoutAndModifyState true getRootElementRef

handleAction
  :: forall slots output action
   . Boolean
  -> H.HalogenM RippleState RippleAction__Unbounded slots output Aff (Maybe Web.HTML.HTMLElement)
  -> RippleAction__Unbounded
  -> H.HalogenM RippleState RippleAction__Unbounded slots output Aff Unit
handleAction
  isDisabled
  getRootElementRef
  =
  case _ of
       Initialize -> handleAction__Initialize getRootElementRef
       WindowResized -> layoutAndModifyState true getRootElementRef

       -- | RippleAction__Common rippleAction__Common -> Halogen.Query.HalogenM.mapAction RippleAction__Common $ handleAction__Common isDisabled true getRootElementRef rippleAction__Common
       RippleAction__Common rippleAction__Common -> pure unit
