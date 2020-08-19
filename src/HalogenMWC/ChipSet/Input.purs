module HalogenMWC.ChipSet.Input where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.Chip.Input
import HalogenMWC.Chip.Input as Chip

chipSet :: Array (IProp r i) -> Array (Tuple String (Chip r i)) -> Html r i
chipSet additionalAttributes keyedChips =
    HH.keyed "mdc-chip-set"
        ([ HP.class_ mdc_chip_set, HP.class_ mdc_chip_set____input, HH.Attributes.attribute "role" "grid"] <> additionalAttributes)
        (map (Tuple.mapSecond chip) keyedChips)

chip :: Chip r i -> Html r i
chip (Chip config_ label) =
    HH.div [ HP.class_ mdc_touch_target_wrapper ]
        [ HH.element "mdc-chip"
            (Array.filterMap identity
                [ HP.class_ mdc_chip
                , HP.class_ mdc_chip____touch
                , HH.Attributes.attribute "role" "row"
                , removalHandler config_
                ]
                <> config_.additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt label
                , trailingIconElt config_
                ]
            )
        ]

tabIndexProp :: Int -> IProp r i
tabIndexProp tabIndex =
    HH.Attributes.property "tabIndex" (Encode.int tabIndex)

removalHandler :: Chip.Config r i -> Maybe (IProp r i)
removalHandler { onDelete } =
    map (HH.Events.on "MDCChip:removal" << Decode.succeed) onDelete

rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_chip__ripple ] [])

leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt { leadingIcon } =
    map
        (\iconName ->
            HH.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        leadingIcon

primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just $
        HH.span [ HP.class_ mdc_chip__primary_action, HH.Attributes.attribute "role" "gridcell", tabIndexProp -1 ]
            (Array.filterMap identity [ textElt label, touchElt ])

textElt :: String -> Maybe (Html r i)
textElt label =
    Just (HH.span [ HP.class_ mdc_chip__text, HH.Attributes.attribute "role" "button" ] [ text label ])

touchElt :: Maybe (Html r i)
touchElt =
    Just (HH.div [ HP.class_ mdc_chip__touch ] [])

trailingIconElt :: Chip.Config r i -> Maybe (Html r i)
trailingIconElt { trailingIcon, onDelete } =
    if onDelete /= Nothing then
        Just $
            HH.i
                [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--trailing"
                , tabIndexProp -1
                , HH.Attributes.attribute "role" "button"
                ]
                [ text (Maybe.withDefault "cancel" trailingIcon) ]

    else
        Nothing
