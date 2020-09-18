module HalogenMWC.Implementation.Ripple.Bounded where

import Protolude

import Halogen as H
import Web.HTML as Web.HTML
import HalogenMWC.Ripple.Common (RippleAction__Common, RippleState, handleAction__Common)

handleAction
  :: forall slots output
   . Boolean
  -> H.HalogenM RippleState RippleAction__Common slots output Aff (Maybe Web.HTML.HTMLElement)
  -> RippleAction__Common
  -> H.HalogenM RippleState RippleAction__Common slots output Aff Unit
handleAction isDisabled = handleAction__Common isDisabled false
