module Demo.Pages.Selects.Hero where

import Protolude
import Halogen as H
import Halogen.HTML as HH
import HalogenMWC.Chip.Choice as Chip.Choice
import HalogenMWC.ChipSet.Choice as ChipSet.Choice
import Demo.Pages.Selects.Shared
import HalogenMWC.Select as Select
import HalogenMWC.Select.Item as Select.Item

type State =
  { hero :: Maybe Fruit
  }

data Action
  = HeroChanged (Maybe Fruit)

initialState :: forall r w i . State
initialState =
  { hero: Nothing
  }

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
    case _ of
        HeroChanged hero -> H.modify_ (_ { hero = hero } )

render :: forall m. State -> HH.ComponentHTML Action ChildSlots m
render state =
  Select.select Select.Filled
    (Select.defaultConfig
        { label = Just "Fruit"
        , selected = Just state.hero
        , onChange = Just HeroChanged
        }
    )
    firstItem
    remainingItems

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
