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
import Halogen (AttrName(..), ElemName(..), PropName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Implementation.TextField.FilledShared (LineRippleState(..))
import HalogenMWC.Implementation.TextField.FilledShared (LineRippleState(..)) as FilledShared
import HalogenMWC.Implementation.TextField.Input (Config) as Export
import HalogenMWC.Implementation.TextField.Input as TextField.Input
import HalogenMWC.Implementation.TextField.Shared (LabelConfig(..)) as Export
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

type ConfigManagedByComponentInternal r =
  ( value :: String
  | r )

type Input = Record (TextField.Input.ConfigManagedByUser + ConfigManagedByComponentInternal + ())

type Message = Void

type State = Record (TextField.Input.ConfigManagedByUser + TextField.Input.ConfigManagedByComponent + ConfigManagedByComponentInternal + ())

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
  }

filled :: H.Component Query Input Message Aff
filled =
  H.mkComponent
    { initialState: \input -> Record.union input
      { focused: false
      , lineRippleState: FilledShared.LineRippleState__Idle
      }
    , render: \state ->
        HH.div_ $ TextField.Input.filled $ Record.Builder.build
          ( Record.Builder.delete (SProxy :: SProxy "value") >>>
            Record.Builder.union
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
    setEfficientlyStateFocused :: Boolean -> _ -> _
    setEfficientlyStateFocused new state =
      if state.focused == new
        then state
        else state { focused = new }

    setEfficientlyStateValue :: String -> _ -> _
    setEfficientlyStateValue new state =
      if state.value == new
        then state
        else state { value = new }

    setEfficientlyStateLineRippleState :: FilledShared.LineRippleState -> _ -> _
    setEfficientlyStateLineRippleState new state =
      case new, state.lineRippleState of
           FilledShared.LineRippleState__Idle, FilledShared.LineRippleState__Idle -> state
           FilledShared.LineRippleState__Active _, FilledShared.LineRippleState__Active _ -> state -- probably animation already finished, no need to set new center point
           _, _ -> state { lineRippleState = new }

    handlePointerDown :: Int -> H.HalogenM State Action ChildSlots Message Aff Unit
    handlePointerDown clientX = do
      state <- H.get

      if state.disabled then pure unit else
        H.getHTMLElementRef inputRefLabel >>= traverse_ \(inputElement :: HTMLElement) -> do
           (inputElementDomRect :: Web.HTML.HTMLElement.DOMRect) <- H.liftEffect $ Web.HTML.HTMLElement.getBoundingClientRect inputElement
           let normalizedX = Int.toNumber clientX - inputElementDomRect.left

           H.modify_ $
             setEfficientlyStateLineRippleState (FilledShared.LineRippleState__Active normalizedX)
             >>> setEfficientlyStateFocused true

    handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
    handleAction =
      case _ of
            Action__Focus -> H.modify_ $ setEfficientlyStateFocused true
            Action__Blur -> H.modify_ $ setEfficientlyStateFocused false
            Action__Input input -> H.modify_ $ setEfficientlyStateValue input

            -- only for filled
            Action__PointerDown__Mouse mouseEvent -> handlePointerDown (Web.UIEvent.MouseEvent.clientX mouseEvent)
            Action__PointerDown__Touch touchEvent -> do
              case Web.TouchEvent.TouchList.item 0 $ Web.TouchEvent.TouchEvent.changedTouches touchEvent of
                   Just touchEventItem -> handlePointerDown (Web.TouchEvent.Touch.clientX touchEventItem)
                   _ -> pure unit
