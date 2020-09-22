module HalogenMWC.TextField.Filled where

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
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Core (ClassName)
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Utils as Utils
import Material.Classes.LineRipple
import Material.Classes.Textfield
import MaterialIconsFont.Classes
import HalogenMWC.Implementation.TextField.Input as TextField.Input

type Query = Const Void

type Input = Void

type Message = Void

type State = TextField.Input.Config

data Action
  = Action__Focus
  | Action__Blur
  | Action__Input
  | Action__PointerDown

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
              , HE.onInput (const Action__Input)

              , HE.onMouseDown (const Action__PointerDown)
              , HE.onTouchStart (const Action__PointerDown)
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
             HH.modify \state -> state
              { focused = true
              , lineRippleState = LineRippleState__Idle
              }

            -- | this.isFocused_ = true;
            -- | this.styleFocused_(this.isFocused_);
            -- | this.adapter.activateLineRipple();
            -- | if (this.adapter.hasLabel()) {
            -- |   this.notchOutline(this.shouldFloat);
            -- |   this.adapter.floatLabel(this.shouldFloat);
            -- |   this.styleFloating_(this.shouldFloat);
            -- |   this.adapter.shakeLabel(this.shouldShake);
            -- | }
            -- | if (this.helperText_) {
            -- |   this.helperText_.showToScreenReader();
            -- | }
            Action__Blur ->
              HH.modify \state -> state
               { focused = true
               , lineRippleState = LineRippleState__Idle
               }

              -- | this.isFocused_ = false;
              -- | this.adapter.deactivateLineRipple();
              -- | const isValid = this.isValid();
              -- | this.styleValidity_(isValid);
              -- | this.styleFocused_(this.isFocused_);
              -- | if (this.adapter.hasLabel()) {
              -- |   this.notchOutline(this.shouldFloat);
              -- |   this.adapter.floatLabel(this.shouldFloat);
              -- |   this.styleFloating_(this.shouldFloat);
              -- |   this.adapter.shakeLabel(this.shouldShake);
              -- | }
              -- | if (!this.shouldFloat) {
              -- |   this.receivedUserInput_ = false;
              -- | }

           Click -> H.raise Clicked
