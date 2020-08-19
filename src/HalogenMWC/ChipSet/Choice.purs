module HalogenMWC.ChipSet.Choice where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Choice

type Config a r i =
  { selected :: Maybe a
  , onChange :: Maybe (a -> r i)
  , toLabel :: a -> String
  , additionalAttributes :: Array (IProp r i)
  }

config :: { toLabel :: a -> String } -> Config a r i
config { toLabel } =
        { selected: Nothing
        , onChange: Nothing
        , toLabel: toLabel
        , additionalAttributes: []
        }

chipSet :: Config a r i -> Array (Chip a r i) -> Html r i
chipSet (config_@{ selected, onChange, toLabel, additionalAttributes }) chips =
    HH.element "mdc-chip-set"
        ([ chipSetCs, chipSetChoiceCs, gridRole] <> additionalAttributes)
        (map (chip selected onChange toLabel) chips)

chip :: Maybe a -> Maybe (a -> r i) -> (a -> String) -> Chip a r i -> Html r i
chip selected onChange toLabel (Chip config_ value) =
    HH.div [ HP.class_ mdc_touch_target_wrapper ]
        [ HH.element "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , selectedProp (Just value == selected)
                , interactionHandler (map ((#) value) onChange)
                ]
                <> config_.additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt (toLabel value)
                ]
            )
        ]

chipSetCs :: IProp r i
chipSetCs =
    HP.class_ mdc_chip_set

chipSetChoiceCs :: IProp r i
chipSetChoiceCs =
    HP.class_ mdc_chip_set____choice

gridRole :: IProp r i
gridRole =
    HH.Attributes.attribute "role" "grid"

chipCs :: Maybe (IProp r i)
chipCs =
    Just (HP.class_ mdc_chip)

chipTextCs :: IProp r i
chipTextCs =
    HP.class_ mdc_chip__text

chipTouchCs :: Maybe (IProp r i)
chipTouchCs =
    Just (HP.class_ mdc_chip____touch)

chipPrimaryActionCs :: IProp r i
chipPrimaryActionCs =
    HP.class_ mdc_chip__primary_action

selectedProp :: Boolean -> Maybe (IProp r i)
selectedProp selected =
    Just (HH.Attributes.property "selected" (Encode.bool selected))

buttonRole :: IProp r i
buttonRole =
    HH.Attributes.attribute "role" "button"

rowRole :: Maybe (IProp r i)
rowRole =
    Just (HH.Attributes.attribute "role" "row")

gridcellRole :: IProp r i
gridcellRole =
    HH.Attributes.attribute "role" "gridcell"

interactionHandler :: Maybe r i -> Maybe (IProp r i)
interactionHandler r i =
    map (HH.Events.on "MDCChip:interaction" << Decode.succeed) r i

rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Config r i -> Maybe (Html r i)
leadingIconElt { icon } =
    map
        (\iconName ->
            HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        icon

primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just $
        HH.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])

textElt :: String -> Maybe (Html r i)
textElt label =
    Just (HH.span [ chipTextCs, buttonRole ] [ text label ])

touchElt :: Maybe (Html r i)
touchElt =
    Just (HH.div [ HP.class_ mdc_chip__touch ] [])
