module HalogenMWC.ChipSet.Input (chipSet) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA


import HH.Keyed


import HalogenMWC.Chip.Input.Internal as Chip



chipSet :: Array (IProp r i) -> Array ( String, Chip r i ) -> Html r i
chipSet additionalAttributes keyedChips =
    HH.Keyed.node "mdc-chip-set"
        ([ chipSetCs, chipSetInputCs, gridRole] <> additionalAttributes)
        (Array.map (Tuple.mapSecond chip) keyedChips)


chip :: Chip r i -> Html r i
chip (Chip (config_@(Chip.Config { additionalAttributes })) label) =
    HH.div [ HP.class_ mdc_touch_target_wrapper ]
        [ HH.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , removalHandler config_
                ]
                <> additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt label
                , trailingIconElt config_
                ]
            )
        ]


chipSetCs :: HH.Attribute r i
chipSetCs =
    HP.class_ mdc_chip_set


chipSetInputCs :: HH.Attribute r i
chipSetInputCs =
    HP.class_ mdc_chip_set____input


gridRole :: HH.Attribute r i
gridRole =
    HH.Attributes.attribute "role" "grid"


chipCs :: Maybe (HH.Attribute r i)
chipCs =
    Just (HP.class_ mdc_chip)


chipTextCs :: HH.Attribute r i
chipTextCs =
    HP.class_ mdc_chip__text


chipTouchCs :: Maybe (HH.Attribute r i)
chipTouchCs =
    Just (HP.class_ mdc_chip____touch)


chipPrimaryActionCs :: HH.Attribute r i
chipPrimaryActionCs =
    HP.class_ mdc_chip__primary_action


tabIndexProp :: Int -> HH.Attribute r i
tabIndexProp tabIndex =
    HH.Attributes.property "tabIndex" (Encode.int tabIndex)


buttonRole :: HH.Attribute r i
buttonRole =
    HH.Attributes.attribute "role" "button"


rowRole :: Maybe (HH.Attribute r i)
rowRole =
    Just (HH.Attributes.attribute "role" "row")


gridcellRole :: HH.Attribute r i
gridcellRole =
    HH.Attributes.attribute "role" "gridcell"


removalHandler :: Chip.Config r i -> Maybe (HH.Attribute r i)
removalHandler (Chip.Config { onDelete }) =
    Maybe.map (HH.Events.on "MDCChip:removal" << Decode.succeed) onDelete


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_chip__ripple ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { leadingIcon }) =
    Maybe.map
        (\iconName ->
            HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        leadingIcon


primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just $
        HH.span [ chipPrimaryActionCs, gridcellRole, tabIndexProp -1 ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (HH.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (HH.div [ HP.class_ mdc_chip__touch ] [])


trailingIconElt :: Chip.Config r i -> Maybe (Html r i)
trailingIconElt (Chip.Config { trailingIcon, onDelete }) =
    if onDelete /= Nothing then
        Just $
            HH.i
                [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--trailing"
                , tabIndexProp -1
                , buttonRole
                ]
                [ text (Maybe.withDefault "cancel" trailingIcon) ]

    else
        Nothing
