module Demo.Pages.Checkbox.Hero where

import Demo.Pages.Checkbox.Shared
import Data.Map
import Protolude
import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Button as Button
import HalogenMWC.Checkbox as Checkbox
import Material.Classes.Typography
import Data.Lens (Lens')
import Data.Lens as Lens
import Data.Lens.Record as Lens
import Demo.Utils

type State =
  { "checked-hero-checkbox" :: Maybe Checkbox.State
  , "unchecked-hero-checkbox" :: Maybe Checkbox.State
  }

type Input = Unit

initialState :: State
initialState =
  { "checked-hero-checkbox": Just Checkbox.Checked
  , "unchecked-hero-checkbox": Just Checkbox.Unchecked
  }

prop_checked_hero_checkbox   = Lens.prop (SProxy :: SProxy "checked-hero-checkbox")
prop_unchecked_hero_checkbox = Lens.prop (SProxy :: SProxy "unchecked-hero-checkbox")

render :: forall m . State -> HH.ComponentHTML (Action State) ChildSlots m
render state =
  HH.div_
    [ checkbox prop_checked_hero_checkbox state heroMargin
    , checkbox prop_unchecked_hero_checkbox state heroMargin
    ]

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

heroMargin :: forall r i . Array (IProp ( style :: String | r ) i)
heroMargin =
    [ HP.style "margin: 8px 16px;" ]
