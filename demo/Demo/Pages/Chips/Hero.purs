module Demo.Pages.Chips.Hero where

import Data.Set (Set)
import Data.Set as Set
import Demo.HOC.CatalogPage
import Halogen
import Halogen
import Material.Classes.Typography
import Protolude
import Protolude

import Data.Array as Array
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Maybe as Maybe
import Halogen as H
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.Chip.Action as Chip.Action
import HalogenMWC.Chip.Choice as Chip.Choice
import HalogenMWC.Chip.Filter as Chip.Filter
import HalogenMWC.Chip.Input as Chip.Input
import HalogenMWC.ChipSet.Action as ChipSet.Action
import HalogenMWC.ChipSet.Choice as ChipSet.Choice
import HalogenMWC.ChipSet.Filter as ChipSet.Filter
import HalogenMWC.ChipSet.Input as ChipSet.Input
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as Web.UIEvent.KeyboardEvent
import Data.String as String
import Demo.Utils

type State =
  { chip :: Maybe String
  }

type Input = Unit

initialState :: State
initialState =
  { chip: Just "Chip One"
  }

data Action
  = ChipChanged String

render :: forall m . State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div_
  [ ChipSet.Choice.chipSet
      ((ChipSet.Choice.defaultConfig { toLabel: identity })
        { selected = state.chip
        , onChange = Just ChipChanged
        }
      )
      [ Chip.Choice.Chip Chip.Choice.defaultConfig "Chip One"
      , Chip.Choice.Chip Chip.Choice.defaultConfig "Chip Two"
      , Chip.Choice.Chip Chip.Choice.defaultConfig "Chip Three"
      , Chip.Choice.Chip Chip.Choice.defaultConfig "Chip Four"
      ]
  ]

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
  case _ of
    ChipChanged chip -> H.modify_ (_ { chip = Just chip })

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
