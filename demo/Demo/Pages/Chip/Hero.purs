module Demo.Pages.Chip.Hero where

import Demo.HOC.CatalogPage (ChildSlots, Message, Query)
import Protolude (Aff, Maybe(..), Unit, const, identity)
import Halogen as H
import Halogen.HTML as HH
import HalogenMWC.Chip.Choice as Chip.Choice
import HalogenMWC.ChipSet.Choice as ChipSet.Choice

type State
  = { chip :: Maybe String
    }

type Input
  = Unit

initialState :: State
initialState =
  { chip: Just "Chip One"
  }

data Action
  = ChipChanged String

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  HH.div_
    [ ChipSet.Choice.chipSet
        ( (ChipSet.Choice.defaultConfig { toLabel: identity })
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
handleAction = case _ of
  ChipChanged chip -> H.modify_ (_ { chip = Just chip })

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
