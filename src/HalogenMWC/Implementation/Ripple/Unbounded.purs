module HalogenMWC.Ripple.Unbounded where

import Protolude

import FRP.Event as Event
import Halogen as H
import HalogenMWC.Utils as Utils
import Web.Event.Event (EventType(..))
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import HalogenMWC.Ripple.Common (RippleAction__Common, RippleState, layoutAndModifyState)

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
