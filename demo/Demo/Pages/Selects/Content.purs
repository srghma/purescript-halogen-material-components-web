module Demo.Pages.Selects.Content where

import Data.Set (Set)
import Data.Set as Set
import Material.Classes.Typography (mdc_typography____body2, mdc_typography____subtitle1)
import Protolude (class Eq, Aff, Maybe(..), Tuple(..), Unit, bind, const, identity, map, pure, unit, unless, when, ($), (-), (/=), (/\))
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
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
import Demo.Utils (focusById)
import Demo.Pages.Selects.Shared
import HalogenMWC.Select as Select
import HalogenMWC.Select.Item as Select.Item

type State =
  { filled :: Maybe Fruit
  , outlined :: Maybe Fruit
  , filledWithIcon :: Maybe Fruit
  , outlinedWithIcon :: Maybe Fruit
  , focused :: Maybe Fruit
  }

data Action
  = FilledChanged (Maybe Fruit)
  | OutlinedChanged (Maybe Fruit)
  | FilledWithIconChanged (Maybe Fruit)
  | OutlinedWithIconChanged (Maybe Fruit)
  | FocusedChanged (Maybe Fruit)
  | Focus String

initialState :: forall r w i . State
initialState =
  { filled: Nothing
  , outlined: Nothing
  , filledWithIcon: Nothing
  , outlinedWithIcon: Nothing
  , focused: Nothing
  }

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction =
    case _ of
        FilledChanged filled -> H.modify_ (_ { filled = filled } )
        OutlinedChanged outlined -> H.modify_ (_ { outlined = outlined } )
        FilledWithIconChanged filledWithIcon -> H.modify_ (_ { filledWithIcon = filledWithIcon } )
        OutlinedWithIconChanged outlinedWithIcon -> H.modify_ (_ { outlinedWithIcon = outlinedWithIcon } )
        FocusedChanged focused -> H.modify_ (_ { focused = focused } )
        Focus id -> H.liftEffect $ focusById id

component :: H.Component Query Input Message Aff
component =
  H.mkComponent
    { initialState: const initialState
    , render: \state ->
        HH.div_
        [ HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Filled" ]
        , filledSelect state
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Outlined" ]
        , outlinedSelect state
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Filled with Icon" ]
        , filledWithIconSelect state
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Outlined with Icon" ]
        , outlinedWithIconSelect state
        , HH.h3 [ HP.class_ mdc_typography____subtitle1 ] [ HH.text "Focus Select" ]
        , focusSelect state
        ]
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

filledSelect :: forall r w i . State -> HH.HTML w Action
filledSelect state =
    Select.select Select.Filled
        (Select.defaultConfig
            { label = Just "Fruit"
            , selected = Just state.filled
            , onChange = Just FilledChanged
            }
        )
        firstItem
        remainingItems

outlinedSelect :: forall r w i . State -> HH.HTML w Action
outlinedSelect state =
    Select.select Select.Outlined
        (Select.defaultConfig
            { label = Just "Fruit"
            , selected = Just state.outlined
            , onChange = Just OutlinedChanged
            }
        )
        firstItem
        remainingItems

filledWithIconSelect :: forall r w i . State -> HH.HTML w Action
filledWithIconSelect state =
    Select.select Select.Filled
        (Select.defaultConfig
            { label = Just "Fruit"
            , selected = Just state.filledWithIcon
            , leadingIcon = Just (Select.icon [] "favorite")
            , onChange = Just FilledWithIconChanged
            }
        )
        firstItem
        remainingItems

outlinedWithIconSelect :: forall r w i . State -> HH.HTML w Action
outlinedWithIconSelect state =
    Select.select Select.Outlined
        (Select.defaultConfig
            { label = Just "Fruit"
            , selected = Just state.outlinedWithIcon
            , leadingIcon = Just (Select.icon [] "favorite")
            , onChange = Just OutlinedWithIconChanged
            }
        )
        firstItem
        remainingItems

focusSelect :: forall r w i . State -> HH.HTML w Action
focusSelect state =
    HH.div
        [ HP.style "display: flex; align-items: center;"
        ]
        [ Select.select Select.Filled
            (Select.defaultConfig
                { selected = Just state.focused
                , onChange = Just FocusedChanged
                , additionalAttributes = [ HP.id_ "my-select" ]
                }
            )
            firstItem
            remainingItems
        , HH.text "\x00A0"
        , Button.button Button.Raised
            (Button.defaultConfig { additionalAttributes = [ HE.onClick (const $ Focus "my-select") ] })
            [ HH.text "Focus" ]
        ]