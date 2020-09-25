module HalogenMWC.TextField.Outlined
  ( module HalogenMWC.TextField.Outlined
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
import HalogenMWC.Implementation.TextField.View.Shared (ActivationState(..))
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
import HalogenMWC.Implementation.TextField.Component.Shared
import HalogenMWC.Implementation.TextField.Component.Shared (Message(..), Query(..), Input, State, Action(..), defaultConfig) as Export

outlined :: H.Component Query Input Message Aff
outlined =
  H.mkComponent
    { initialState: \input -> Record.union input
      { activationState: ActivationState__Idle
      }
    , render: \state -> trace { message: "render outlined", state } $ const $ HH.div_ $ TextField.Input.outlined $ Record.union state additionalAttributes
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = \input -> Just $ Action__Receive input
        }
      }
