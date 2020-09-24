module HalogenMWC.TextField.Filled
  ( module HalogenMWC.TextField.Filled
  , module Export
  ) where

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
import HalogenMWC.Implementation.TextField.Shared (FocusState(..))
import HalogenMWC.Implementation.TextField.Input (Config) as Export
import HalogenMWC.Implementation.TextField.Input as TextField.Input
import HalogenMWC.Implementation.TextField.Shared (LabelConfig(..)) as Export
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

type Message = Void

type State = Record (TextField.Input.ConfigManagedByUser + TextField.Input.ConfigManagedByComponent + ())

data Action
  = Action__Focus
  | Action__Blur
  | Action__Input String
  | Action__PointerDown__Mouse MouseEvent
  | Action__PointerDown__Touch TouchEvent

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

  , shake: false
  , required: false
  }

filled :: H.Component Query Input Message Aff
filled =
  H.mkComponent
    { initialState: \input -> Record.union input
      { focusState: FocusState__Idle
      }
    , render: \state ->
        trace { message: "render", state } $ const $ HH.div_ $ TextField.Input.filled $ Record.Builder.build
          ( Record.Builder.union
              { additionalAttributesRoot:
                  [ HE.onMouseDown Action__PointerDown__Mouse
                  , HE.onTouchStart Action__PointerDown__Touch
                  ]
              , additionalAttributesInput:
                  -- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-textfield/foundation.ts#L151
                  [ HE.onFocus (const Action__Focus)
                  , HE.onBlur (const Action__Blur)
                  , HE.onValueInput Action__Input
                  , HP.ref inputRefLabel
                  , HP.value state.value
                  ]
              }
          )
          state
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
      }
  where
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
                  H.modify_ $ setEfficientlyStateFocusState (FocusState__Active (Just normalizedX))

    handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
    handleAction action =
      case spy "action" action of
            Action__Focus -> H.modify_ $ setEfficientlyStateFocusState (FocusState__Active Nothing)
            Action__Blur -> H.modify_ $ setEfficientlyStateFocusState FocusState__Idle
            Action__Input input -> H.modify_ $ setEfficientlyStateValue input

            -- only for filled
            Action__PointerDown__Mouse mouseEvent -> handlePointerDown (Just $ Web.UIEvent.MouseEvent.clientX mouseEvent)
            Action__PointerDown__Touch touchEvent -> do
              case Web.TouchEvent.TouchList.item 0 $ Web.TouchEvent.TouchEvent.changedTouches touchEvent of
                   Just touchEventItem -> handlePointerDown (Just $ Web.TouchEvent.Touch.clientX touchEventItem)
                   _ -> handlePointerDown Nothing
