module HalogenMWC.Ripple.HTML where

import HalogenMWC.Ripple.Common
import Protolude

import Data.Array as Array
import Data.String as String
import Halogen (ComponentSlot, ElemName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events (onKeyUp)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation as Implementation
import HalogenMWC.Button.Insides as Insides
import HalogenMWC.Ripple.Common as Ripple
import HalogenMWC.Ripple.Constants as Ripple
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
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

styleVar :: String -> String -> String
styleVar x y = x <> ": " <> y


rippleClasses :: Boolean -> RippleState -> Array ClassName
rippleClasses isUnbounded rippleState =
  Array.concat
  [ [ Ripple.cssClasses."ROOT" ]
  , if isUnbounded then [ Ripple.cssClasses."UNBOUNDED" ] else []
  , if rippleState.focused then [ Ripple.cssClasses."BG_FOCUSED" ] else []
  , case rippleState.activationState of
         Ripple.ActivationState__Idle -> []
         Ripple.ActivationState__Activated -> [ Ripple.cssClasses."FG_ACTIVATION" ]
         Ripple.ActivationState__Deactivated -> [ Ripple.cssClasses."FG_DEACTIVATION" ]
  ]

rippleStyles :: RippleState -> String
rippleStyles rippleState =
  String.joinWith "; " $
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
  -- | , HE.onKeyUp (const Deactivate) -- only when onKeyDown worked?

  , HE.onFocus (const Ripple.Focus)
  , HE.onBlur (const Ripple.Blur)
  ]
