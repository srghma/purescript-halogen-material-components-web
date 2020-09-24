module HalogenMWC.Implementation.TextField.Component.Shared where

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
import HalogenMWC.Implementation.TextField.View.Shared (FocusState(..))
import HalogenMWC.Implementation.TextField.View.Input (Config) as Export
import HalogenMWC.Implementation.TextField.View.Input as TextField.Input
import HalogenMWC.Implementation.TextField.View.Shared (LabelConfig(..)) as Export
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

type Query = Const Void

type Input = Record (TextField.Input.ConfigManagedByUser + ())

data Message
  = Message__Input String

type State = Record (TextField.Input.ConfigManagedByUser + TextField.Input.ConfigManagedByComponent + ())

data Action
  = Action__Focus
  | Action__Blur
  | Action__Input String
  | Action__PointerDown__Mouse MouseEvent
  | Action__PointerDown__Touch TouchEvent
  | Action__Receive Input

type ChildSlots = ()

inputRefLabel :: H.RefLabel
inputRefLabel = H.RefLabel "input"

defaultConfig :: _ -> Input
defaultConfig { label, value } =
  { label
  , value

  , placeholder: Nothing
  , type_:       InputText

  , disabled:  false
  , fullwidth: false
  , invalid:   false

  , helperText:       Nothing
  , characterCounter: Nothing

  , minLength:                 Nothing
  , prefix:                    Nothing
  , suffix:                    Nothing

  , additionalClassesRoot:     []
  , additionalClassesInput:    []

  , shake: false -- remove and use 'invalid' instead?
  , required: false
  }

additionalAttributes =
  { additionalAttributesRoot
  , additionalAttributesInput
  }
  where
    additionalAttributesRoot  :: Array (IProp I.HTMLlabel Action)
    additionalAttributesRoot =
      [ HE.onMouseDown Action__PointerDown__Mouse
      , HE.onTouchStart Action__PointerDown__Touch
      ]

    additionalAttributesInput :: Array (IProp I.HTMLinput Action)
    additionalAttributesInput =
      -- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-textfield/foundation.ts#L151
      [ HE.onFocus (const Action__Focus)
      , HE.onBlur (const Action__Blur)
      , HE.onValueInput Action__Input
      , HP.ref inputRefLabel
      ]

setEfficientlyStateValue :: String -> State -> State
setEfficientlyStateValue = setEfficiently (Lens.prop (SProxy :: SProxy "value"))

setEfficientlyStateFocusState :: FocusState -> State -> State
setEfficientlyStateFocusState = setEfficientlyCustomEq
  (\old new ->
    case old, new of
         FocusState__Idle, FocusState__Idle -> true
         FocusState__Active _, FocusState__Active _ -> true -- probably animation already finished, no need to set new center point
         _, _ -> false
  )
  (Lens.prop (SProxy :: SProxy "focusState"))

handlePointerDown :: Maybe Int -> H.HalogenM State Action ChildSlots Message Aff Unit
handlePointerDown mclientX = do
  state <- H.get

  if state.disabled then pure unit else
    H.getHTMLElementRef inputRefLabel >>= traverse_ \(inputElement :: HTMLElement) ->
       case mclientX of
            -- TODO:
            -- the transform-origin doesn't work, when on span.mdc-line-ripple,
            -- but works when on span.mdc-line-ripple::after

            Nothing -> H.modify_ $ setEfficientlyStateFocusState (FocusState__Active Nothing)
            Just clientX -> do
              (inputElementDomRect :: Web.HTML.HTMLElement.DOMRect) <- H.liftEffect $ Web.HTML.HTMLElement.getBoundingClientRect inputElement
              let normalizedX = Int.toNumber clientX - inputElementDomRect.left

              -- TODO:
              -- not needed for outlined actually, only for filled, can set Nothing
              H.modify_ $ setEfficientlyStateFocusState (FocusState__Active (Just normalizedX))

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction action =
  case spy "action" action of
        Action__Receive newInput -> H.modify_ \state -> Record.merge state newInput -- TODO: can be optimized?

        Action__Focus -> H.modify_ $ setEfficientlyStateFocusState (FocusState__Active Nothing)
        Action__Blur -> H.modify_ $ setEfficientlyStateFocusState FocusState__Idle
        Action__Input input -> H.raise $ Message__Input input

        -- only for filled
        Action__PointerDown__Mouse mouseEvent -> handlePointerDown (Just $ Web.UIEvent.MouseEvent.clientX mouseEvent)
        Action__PointerDown__Touch touchEvent -> do
          case Web.TouchEvent.TouchList.item 0 $ Web.TouchEvent.TouchEvent.changedTouches touchEvent of
               Just touchEventItem -> handlePointerDown (Just $ Web.TouchEvent.Touch.clientX touchEventItem)
               _ -> handlePointerDown Nothing
