module Demo.Pages.Checkbox.Content where

import Demo.Pages.Checkbox.Shared (Action(..), ChildSlots, Message, Query, checkbox, handleAction)
import Protolude (Aff, Maybe(..), SProxy(..), Unit, const, ($))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import HalogenMWC.Button as Button
import HalogenMWC.Checkbox as Checkbox
import Material.Classes.Typography (mdc_typography____subtitle1)
import Data.Lens (Lens')
import Data.Lens.Record (prop) as Lens

type State
  = { "unchecked-checkbox" :: Maybe Checkbox.State
    , "indeterminate-checkbox" :: Maybe Checkbox.State
    , "checked-checkbox" :: Maybe Checkbox.State
    }

type Input
  = Unit

initialState :: State
initialState =
  { "unchecked-checkbox": Just Checkbox.Unchecked
  , "indeterminate-checkbox": Just Checkbox.Checked
  , "checked-checkbox": Just Checkbox.Checked
  }

prop_unchecked_checkbox :: Lens' State (Maybe Checkbox.State)
prop_unchecked_checkbox = Lens.prop (SProxy :: SProxy "unchecked-checkbox")

prop_indeterminate_checkbox :: Lens' State (Maybe Checkbox.State)
prop_indeterminate_checkbox = Lens.prop (SProxy :: SProxy "indeterminate-checkbox")

prop_checked_checkbox :: Lens' State (Maybe Checkbox.State)
prop_checked_checkbox = Lens.prop (SProxy :: SProxy "checked-checkbox")

render :: forall m. State -> HH.ComponentHTML (Action State) ChildSlots m
render state =
  HH.div_
    [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Unchecked" ]
    , checkbox prop_unchecked_checkbox state []
    , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Indeterminate" ]
    , checkbox prop_indeterminate_checkbox state []
    , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Checked" ]
    , checkbox prop_checked_checkbox state []
    , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Checkbox" ]
    , focusCheckbox
    ]

focusCheckbox :: forall w. HH.HTML w (Action State)
focusCheckbox =
  HH.div_
    [ Checkbox.checkbox
        ( Checkbox.defaultConfig
            { additionalAttributes = [ HP.id_ "my-checkbox" ]
            }
        )
    , HH.text "\x00A0"
    , Button.button
        Button.Raised
        (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-checkbox") ] })
        [ HH.text "Focus" ]
    ]

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }
