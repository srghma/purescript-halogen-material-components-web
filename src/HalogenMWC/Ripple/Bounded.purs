module HalogenMWC.Ripple.Bounded
  ( module HalogenMWC.Ripple.Bounded
  , module Export
  ) where

import Protolude

import Data.Array (concat) as Array
import Halogen (ClassName(..))
import Halogen as H
import HalogenMWC.Implementation.Ripple.Common (RippleState, initialRippleState) as Export
import HalogenMWC.Implementation.Ripple.HTML (rippleStyles) as Export
import HalogenMWC.Implementation.Ripple.HTML as HalogenMWC.Implementation.Ripple.HTML
import HalogenMWC.Implementation.Ripple.Common
import Web.HTML as Web.HTML
import HalogenMWC.Implementation.Ripple.Constants
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import Halogen.HTML (IProp)
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

newtype RippleAction = RippleAction RippleAction__Common

handleAction
  :: forall slots output
   . Boolean
  -> Web.HTML.HTMLElement
  -> RippleAction
  -> H.HalogenM RippleState RippleAction slots output Aff Unit
handleAction isDisabled rootElement (RippleAction rippleAction__common) =
  Halogen.Query.HalogenM.mapAction RippleAction $ handleAction__Common isDisabled false rootElement rippleAction__common

rippleClasses :: RippleState -> Array ClassName
rippleClasses rippleState =
  Array.concat
  [ [ cssClasses."ROOT" ]
  , if rippleState.focused then [ cssClasses."BG_FOCUSED" ] else []
  , case rippleState.activationState of
         ActivationState__Idle -> []
         ActivationState__Activated -> [ cssClasses."FG_ACTIVATION" ]
         ActivationState__Deactivated -> [ cssClasses."FG_DEACTIVATION" ]
  ]

rippleProps
  :: forall r
   . Array (IProp (onTouchStart :: TouchEvent, onMouseDown :: MouseEvent, onFocus :: FocusEvent, onBlur :: FocusEvent | r) RippleAction)
rippleProps = HalogenMWC.Implementation.Ripple.HTML.rippleProps <#> map RippleAction
