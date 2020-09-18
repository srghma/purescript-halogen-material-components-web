module HalogenMWC.Implementation.Ripple.HTML where

import HalogenMWC.Ripple.Common (RippleAction__Common, RippleState)
import Protolude

import Data.Array as Array
import Data.String as String
import Halogen.HTML (IProp)
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import HalogenMWC.Ripple.Common (ActivationState(..), RippleAction__Common(..), StyleVars(..)) as Ripple
import HalogenMWC.Ripple.Constants (cssClasses, strings) as Ripple
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

styleVar :: String -> String -> String
styleVar x y = x <> ": " <> y <> ";"

rippleClassesBounded :: RippleState -> Array ClassName
rippleClassesBounded rippleState =
  Array.concat
  [ [ Ripple.cssClasses."ROOT" ]
  , if rippleState.focused then [ Ripple.cssClasses."BG_FOCUSED" ] else []
  , case rippleState.activationState of
         Ripple.ActivationState__Idle -> []
         Ripple.ActivationState__Activated -> [ Ripple.cssClasses."FG_ACTIVATION" ]
         Ripple.ActivationState__Deactivated -> [ Ripple.cssClasses."FG_DEACTIVATION" ]
  ]

rippleClassesUnbounded :: RippleState -> Array ClassName
rippleClassesUnbounded rippleState =
  if isUnbounded then [ Ripple.cssClasses."UNBOUNDED" ] else []
  <> rippleClassesBounded

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
