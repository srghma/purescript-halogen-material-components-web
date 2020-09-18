module HalogenMWC.Implementation.Ripple.HTML where

import HalogenMWC.Implementation.Ripple.Common (RippleAction__Common, RippleState)
import Protolude

import Data.Array as Array
import Data.String as String
import Halogen.HTML (IProp)
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import HalogenMWC.Implementation.Ripple.Common (ActivationState(..), RippleAction__Common(..), StyleVars(..)) as Ripple
import HalogenMWC.Implementation.Ripple.Constants (cssClasses, strings) as Ripple
import HalogenMWC.Utils
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

rippleStyles :: RippleState -> String
rippleStyles rippleState =
  String.joinWith " " $
  [ styleVar Ripple.strings."VAR_FG_SCALE" rippleState.styleCommonVars."VAR_FG_SCALE"
  , styleVar Ripple.strings."VAR_FG_SIZE" rippleState.styleCommonVars."VAR_FG_SIZE"
  ] <>
    case rippleState.styleVars of
          Ripple.StyleVars__Empty -> []
          Ripple.StyleVars__Unbounded styleVars__Unbounded ->
            [ styleVar Ripple.strings."VAR_LEFT" styleVars__Unbounded."VAR_LEFT"
            , styleVar Ripple.strings."VAR_TOP" styleVars__Unbounded."VAR_TOP"
            ]
          Ripple.StyleVars__Bounded styleVars__Bounded ->
            [ styleVar Ripple.strings."VAR_FG_TRANSLATE_END" styleVars__Bounded."VAR_FG_TRANSLATE_END"
            , styleVar Ripple.strings."VAR_FG_TRANSLATE_START" styleVars__Bounded."VAR_FG_TRANSLATE_START"
            ]

rippleProps
  :: forall r
   . Array (IProp (onTouchStart :: TouchEvent, onMouseDown :: MouseEvent, onFocus :: FocusEvent, onBlur :: FocusEvent | r) RippleAction__Common)
rippleProps =
  [ HE.onTouchStart Ripple.TouchActivate
  , HE.onMouseDown Ripple.MouseActivate
  -- | , HE.onKeyDown (const KeyActivate)
  -- | , HE.onKeyUp (const KeyDeactivate) -- register only when onKeyDown worked?

  , HE.onFocus (const Ripple.Focus)
  , HE.onBlur (const Ripple.Blur)
  ]
