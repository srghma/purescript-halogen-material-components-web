module HalogenMWC.Implementation.TextField.Component.Shared where

import HalogenMWC.Implementation.TextField.View.Input
import HalogenMWC.Implementation.TextField.View.Shared
import Material.Classes.LineRipple
import Material.Classes.Textfield
import MaterialIconsFont.Classes
import Protolude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array as Array
import Data.Int as Int
import Data.Lens.Record as Lens
import Halogen (AttrName(..), ElemName(..), HalogenM(..), HalogenQ, PropName(..))
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as Halogen.Query.HalogenM
import HalogenMWC.Utils (setEfficiently, setEfficientlyCustomEq)
import HalogenMWC.Utils as Utils
import Prim.Row (class Nub, class Union)
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

data Message
  = Message__Input String

type ChildSlots = ()

initialState :: ∀ t14 t16. Union t16 ( focused :: Boolean ) t14 ⇒ Record t16 → Record t14
initialState = \input -> Record.union input
  { focused: false
  }

eval :: ∀ t57 t60 t64 t73 t79 t80 t81. Union t80 ( focused :: Boolean | t79 ) t81 ⇒ Nub t81 ( focused :: Boolean | t79 ) ⇒ HalogenQ t73 (Action (Record t80)) (Record t80) t64 → HalogenM { focused ∷ Boolean | t79 } (Action (Record t80)) t60 Message t57 t64
eval = H.mkEval $ H.defaultEval
  { handleAction = handleAction
  , receive = \input -> Just $ Action__Receive input
  }

inputRefLabel :: H.RefLabel
inputRefLabel = H.RefLabel "input"

defaultConfigShared :: Record (ConfigManagedByUser + ())
defaultConfigShared =
  { label: LabelConfig__Without "dummy"
  , value: ""

  , placeholder: Nothing
  , type_:       InputText

  , disabled:  false
  , invalid:   false

  , helperText:       Nothing
  , characterCounterOrMaxLength: CharacterCounterOrMaxLength__All_Disabled

  , minLength:                 Nothing
  , prefix:                    Nothing
  , suffix:                    Nothing

  , additionalClassesRoot: []

  , shake: false -- remove and use 'invalid' instead?
  , required: false
  , endAligned: false
  , ltrText: false
  }

setEfficientlyStateFocused :: forall activationState action . Boolean -> _ -> _
setEfficientlyStateFocused = setEfficiently (Lens.prop (SProxy :: SProxy "focused"))

data Action input
  = Action__Focus
  | Action__Blur
  | Action__Input String
  | Action__Receive input

additionalAttributes =
  { additionalAttributesInput
  }
  where
    additionalAttributesInput :: forall input . Array (IProp I.HTMLinput (Action input))
    additionalAttributesInput =
      -- https://github.com/material-components/material-components-web/blob/a3212b2099765947f2a41d71af2cd95fcbca4b97/packages/mdc-textfield/foundation.ts#L151
      [ HE.onFocus (const Action__Focus)
      , HE.onBlur (const Action__Blur)
      , HE.onValueInput Action__Input
      , HP.ref inputRefLabel
      ]

handleAction action =
  case action of
       Action__Receive newInput -> H.modify_ $ Record.merge newInput
       Action__Focus -> H.modify_ $ setEfficientlyStateFocused true
       Action__Blur -> H.modify_ $ setEfficientlyStateFocused false
       Action__Input input -> H.raise $ Message__Input input
