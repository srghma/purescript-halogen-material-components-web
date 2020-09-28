module HalogenMWC.TextField.Filled
  ( module HalogenMWC.TextField.Filled
  , module Export
  ) where

import HalogenMWC.Implementation.TextField.Component.Shared
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
import HalogenMWC.Implementation.TextField.Component.Shared (Message(..), Query(..), Action(..)) as Export
import HalogenMWC.Implementation.TextField.View.Input (ConfigFilled) as Export
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
import HalogenMWC.Implementation.TextField.View.Shared
import HalogenMWC.Implementation.TextField.View.Input

type Input = Record (ConfigManagedByUser + ( fullwidth :: Boolean ))

type State = Record (ConfigManagedByUser + ConfigManagedByComponent + ( fullwidth :: Boolean ))

defaultConfig :: Input
defaultConfig = Record.union defaultConfigShared { fullwidth: false }

filled :: H.Component Query Input Message Aff
filled =
  H.mkComponent
    { initialState
    , render: \state -> TextField.Input.filled $ Record.union state additionalAttributes
    , eval
    }
