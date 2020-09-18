module HalogenMWC.Button.WithRippleCommon where

import Protolude

import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Button.Implementation (Variant)
import HalogenMWC.Button.Implementation (Variant(..), commonClasses, commonHtml, wrapTouch) as Implementation
import HalogenMWC.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Insides
import HalogenMWC.Ripple.Common (RippleAction__Common, RippleState, initialRippleState) as Ripple
import HalogenMWC.Ripple.Bounded (handleAction) as Ripple
import HalogenMWC.Ripple.HTML (rippleClasses, rippleProps, rippleStyles) as Ripple

type Query = Const Void

type Input config w i =
  { variant :: Variant
  , config :: config i
  , content :: Array (HH.HTML w i)
  }

type ChildSlots = ()

type State config w i =
  { input       :: Input config w i
  , rippleState :: Ripple.RippleState
  }

data Action
  = RippleAction Ripple.RippleAction__Common
  | Click

data Message
  = Clicked

buttonRefLabel :: H.RefLabel
buttonRefLabel = H.RefLabel "button"

isUnbounded :: Boolean
isUnbounded = false

initialState input =
  { input
  , rippleState: Ripple.initialRippleState
  }

liftRippleHandleAction state x = Halogen.Query.HalogenM.imapState (\rippleState -> state { rippleState = rippleState }) (\state -> state.rippleState) $ Halogen.Query.HalogenM.mapAction RippleAction $ x
