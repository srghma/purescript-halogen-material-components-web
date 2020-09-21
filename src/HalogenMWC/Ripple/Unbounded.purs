module HalogenMWC.Ripple.Unbounded
  ( module HalogenMWC.Ripple.Unbounded
  , module Export
  ) where

import Protolude

import Data.Array (concat) as Array
import Halogen (ClassName(..))
import FRP.Event as Event
import Halogen as H
import HalogenMWC.Utils as Utils
import Web.Event.Event (EventType(..))
import Web.HTML as Web.HTML
import Web.HTML.Window as Web.HTML.Window
import HalogenMWC.Implementation.Ripple.Common (RippleState, initialRippleState) as Export
import HalogenMWC.Implementation.Ripple.HTML (rippleStyles) as Export
import HalogenMWC.Implementation.Ripple.Common
import HalogenMWC.Implementation.Ripple.Constants
import HalogenMWC.Ripple.Bounded as Bounded
import HalogenMWC.Implementation.Ripple.HTML as HalogenMWC.Implementation.Ripple.HTML
import Halogen.HTML (IProp)
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Halogen.Query.HalogenM as Halogen.Query.HalogenM

data RippleAction
  = Initialize
  | WindowResized
  | RippleAction__Common RippleAction__Common

rippleClasses :: RippleState -> Array ClassName
rippleClasses rippleState = [ cssClasses."UNBOUNDED" ] <> Bounded.rippleClasses rippleState

handleAction
  :: forall slots output action
   . Boolean
  -> Web.HTML.HTMLElement
  -> RippleAction
  -> H.HalogenM RippleState RippleAction slots output Aff Unit
handleAction = \isDisabled rootElement ->
  case _ of
    Initialize -> handleAction__Initialize rootElement
    WindowResized -> layoutAndModifyState true rootElement

    RippleAction__Common rippleAction__Common -> Halogen.Query.HalogenM.mapAction RippleAction__Common $ handleAction__Common isDisabled true rootElement rippleAction__Common
  where
    handleAction__Initialize
      :: forall output slots
      . Web.HTML.HTMLElement
      -> H.HalogenM RippleState RippleAction slots output Aff Unit
    handleAction__Initialize rootElement = do
      -- register WindowResized
      window <- H.liftEffect Web.HTML.window

      let event :: Event.Event RippleAction
          event =
            (Utils.eventListenerEventSourceWithOptions (EventType "resize") Utils.unsafePassiveIfSupportsAddEventListenerOptions (Web.HTML.Window.toEventTarget window))
            <#> const WindowResized

      void $ H.subscribe event

      -- Unbounded ripples need layout logic applied immediately to set coordinates for both shade and ripple
      layoutAndModifyState true rootElement

rippleProps
  :: forall r
   . Array (IProp (onTouchStart :: TouchEvent, onMouseDown :: MouseEvent, onFocus :: FocusEvent, onBlur :: FocusEvent | r) RippleAction)
rippleProps = HalogenMWC.Implementation.Ripple.HTML.rippleProps <#> map RippleAction__Common
