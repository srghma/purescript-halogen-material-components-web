module HalogenMWC.Implementation.Button.WithRippleCommon where

import Protolude

import DOM.HTML.Indexed as I
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.Button.HTML (Variant)
import HalogenMWC.Implementation.Button.HTML (Variant(..), commonClasses, commonHtml, wrapTouch) as Implementation
import HalogenMWC.Implementation.Button.Insides (buttonIconMaterialIcons, buttonLabel) as Insides
import HalogenMWC.Ripple.Bounded as Ripple

type Query = Const Void

type Input config =
  { variant :: Variant
  , config :: config
  , content :: Array (HH.HTML Void Void)
  }

type State config =
  { input       :: Input config
  , rippleState :: Ripple.RippleState
  }

data Action config
  = Action__RippleAction Ripple.RippleAction
  | Action__Click
  | Action__Receive (Input config)

data Message
  = Message__Clicked

buttonRefLabel :: H.RefLabel
buttonRefLabel = H.RefLabel "button"

isUnbounded :: Boolean
isUnbounded = false

initialState :: forall config . Input config -> State config
initialState input =
  { input
  , rippleState: Ripple.initialRippleState
  }

liftRippleHandleAction state x = Halogen.Query.HalogenM.imapState (\rippleState -> state { rippleState = rippleState }) (\state -> state.rippleState) $ Halogen.Query.HalogenM.mapAction Action__RippleAction $ x
