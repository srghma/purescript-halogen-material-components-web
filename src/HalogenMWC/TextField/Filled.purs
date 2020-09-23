module HalogenMWC.TextField.Filled where

import Material.Classes.LineRipple
import Material.Classes.Textfield
import MaterialIconsFont.Classes
import Protolude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Maybe as Maybe
import Halogen (AttrName(..), ElemName(..), PropName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.TextField.Input as TextField.Input
import HalogenMWC.Utils as Utils
import Web.TouchEvent (TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

type Query = Const Void

type Input = Void

type Message = Void

type State = TextField.Input.Config

data Action
  = Action__Focus
  | Action__Blur
  | Action__Input String
  | Action__PointerDown__Mouse MouseEvent
  | Action__PointerDown__Touch TouchEvent

type ChildSlots = ()

filled :: H.Component Query Input Message Aff
filled =
  H.mkComponent
    { initialState: identity
    , render: \state ->
        TextField.Input.filled
          $
          { label:                     state.label
          , placeholder:               state.placeholder
          , type_:                     state.type_
          , disabled:                  state.disabled
          , helperTextId:              state.helperTextId
          , maxLength:                 state.maxLength
          , minLength:                 state.minLength
          , prefix:                    state.prefix
          , suffix:                    state.suffix
          , additionalClassesInput:    state.additionalClassesInput
          , additionalAttributesInput:
              -- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-textfield/foundation.ts#L151
              [ HE.onFocus (const Action__Focus)
              , HE.onBlur (const Action__Blur)
              , HE.onInput Action__Input

              , HE.onMouseDown Action__PointerDown
              , HE.onTouchStart Action__PointerDown
              ]
              <> state.additionalAttributesInput
          , additionalClassesRoot:    state.additionalClassesRoot
          , additionalAttributesRoot:
              -- | [ HE.onClick (const Interaction)
              -- | ]
              -- | <>
              state.additionalAttributesRoot
          }
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }
  where
    handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
    handleAction =
      case _ of
            Action__Focus ->
              H.modify_ \state -> state
              { focused = true
              }
            Action__Blur ->
              H.modify_ \state -> state
              { focused = false
              }
            Action__Input input -> pure unit
            Action__PointerDown__Mouse mouseEvent -> pure unit
            Action__PointerDown__Touch touchEvent -> pure unit
