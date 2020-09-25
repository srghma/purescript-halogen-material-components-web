module HalogenMWC.Implementation.TextField.Component.FilledShared where

import Material.Classes.LineRipple
import Material.Classes.Textfield
import MaterialIconsFont.Classes
import Protolude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Int as Int
import Data.Lens.Record as Lens
import Halogen (AttrName(..), ElemName(..), PropName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.TextField.View.Input as TextField.Input
import HalogenMWC.Utils (setEfficiently, setEfficientlyCustomEq)
import HalogenMWC.Utils as Utils
import Record as Record
import Record.Builder as Record.Builder
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.TouchEvent (TouchEvent)
import Web.TouchEvent.Touch as Web.TouchEvent.Touch
import Web.TouchEvent.TouchEvent as Web.TouchEvent.TouchEvent
import Web.TouchEvent.TouchList as Web.TouchEvent.TouchList
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as Web.UIEvent.MouseEvent
import HalogenMWC.Implementation.TextField.Component.Shared
import HalogenMWC.Implementation.TextField.View.FilledShared

type State = StateShared ActivationState

data Action
  = Action__Focus
  | Action__Blur
  | Action__Input String
  | Action__Receive Input

additionalAttributes =
  { additionalAttributesRoot
  , additionalAttributesInput
  }
  where
    additionalAttributesRoot  :: Array (IProp I.HTMLlabel Action)
    additionalAttributesRoot = []
      -- | [ HE.onMouseDown Action__PointerDown__Mouse
      -- | , HE.onTouchStart Action__PointerDown__Touch
      -- | ]

    additionalAttributesInput :: Array (IProp I.HTMLinput Action)
    additionalAttributesInput =
      -- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-textfield/foundation.ts#L151
      [ HE.onFocus (const Action__Focus)
      , HE.onBlur (const Action__Blur)
      , HE.onValueInput Action__Input
      , HP.ref inputRefLabel
      ]

setEfficientlyStateActivationState :: ActivationState -> State -> State
setEfficientlyStateActivationState = setEfficientlyCustomEq
  (\old new ->
    case old, new of
         ActivationState__Idle, ActivationState__Idle -> true
         ActivationState__Active _, ActivationState__Active _ -> true -- probably animation already finished, no need to set new center point
         _, _ -> false
  )
  (Lens.prop (SProxy :: SProxy "activationState"))

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction action =
  case spy "action" action of
        Action__Receive newInput -> H.modify_ $ Record.merge newInput

        Action__Focus -> H.modify_ $ setEfficientlyStateActivationState ActivationState__Active
        Action__Blur -> H.modify_ $ setEfficientlyStateActivationState ActivationState__Idle
        Action__Input input -> H.raise $ Message__Input input
