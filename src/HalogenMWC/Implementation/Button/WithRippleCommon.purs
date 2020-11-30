module HalogenMWC.Implementation.Button.WithRippleCommon where

import Protolude

import Halogen (HalogenM(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.Button.HTML (ButtonVariant)
import HalogenMWC.Ripple.Bounded as Ripple

type Query = Const Void

type Input config =
  { variant :: ButtonVariant
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

liftRippleHandleAction :: forall output slots config.
  State config
  -> H.HalogenM Ripple.RippleState Ripple.RippleAction slots output Aff Unit
  -> H.HalogenM (State config) (Action config) slots output Aff Unit
liftRippleHandleAction state x =
  Halogen.Query.HalogenM.imapState
  (\rippleState -> state { rippleState = rippleState })
  (\state' -> state'.rippleState)
  $ Halogen.Query.HalogenM.mapAction Action__RippleAction
  $ x
