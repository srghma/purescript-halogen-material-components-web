module HalogenMWC.Ripple.Bounded
  ( module HalogenMWC.Ripple.Bounded
  , module Export
  ) where

import Protolude

import Data.Array (concat) as Array
import Halogen (ClassName)
import Halogen as H
import HalogenMWC.Implementation.Ripple.Common (RippleState, initialRippleState) as Export
import HalogenMWC.Implementation.Ripple.HTML (rippleStyles) as Export
import HalogenMWC.Implementation.Ripple.HTML as HalogenMWC.Implementation.Ripple.HTML
import HalogenMWC.Implementation.Ripple.Common (ActivationState(..), RippleAction__Common, RippleState, handleAction__Common)
import Web.HTML as Web.HTML
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import Halogen.HTML (IProp)
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Material.Classes.Ripple (mdc_ripple_upgraded, mdc_ripple_upgraded____background_focused, mdc_ripple_upgraded____foreground_activation, mdc_ripple_upgraded____foreground_deactivation)

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
  [ [ mdc_ripple_upgraded ]
  , if rippleState.focused then [ mdc_ripple_upgraded____background_focused ] else []
  , case rippleState.activationState of
         ActivationState__Idle -> []
         ActivationState__Activated -> [ mdc_ripple_upgraded____foreground_activation ]
         ActivationState__Deactivated -> [ mdc_ripple_upgraded____foreground_deactivation ]
  ]

rippleProps
  :: forall r
   . Array (IProp (onTouchStart :: TouchEvent, onMouseDown :: MouseEvent, onFocus :: FocusEvent, onBlur :: FocusEvent | r) RippleAction)
rippleProps = HalogenMWC.Implementation.Ripple.HTML.rippleProps <#> map RippleAction
