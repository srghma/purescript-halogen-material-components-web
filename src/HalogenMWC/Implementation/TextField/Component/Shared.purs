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

type Query = Const Void

type Input = Record (TextField.Input.ConfigManagedByUser + ())

data Message
  = Message__Input String

type StateShared activationState = Record (TextField.Input.ConfigManagedByUser + TextField.Input.ConfigManagedByComponent activationState + ())

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

setEfficientlyStateValue :: forall activationState . String -> StateShared activationState -> StateShared activationState
setEfficientlyStateValue = setEfficiently (Lens.prop (SProxy :: SProxy "value"))
