module HalogenMWC.ChipSet.Action (chipSet) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA



import HalogenMWC.Chip.Action.Internal as Chip



chipSet :: Array (IProp r i) -> Array (Chip r i) -> Html r i
chipSet additionalAttributes chips =
    HH.node "mdc-chip-set"
        ([ chipSetCs, chipSetActionCs, gridRole] <> additionalAttributes)
        (Array.map chip chips)


chipSetCs :: HH.Attribute r i
chipSetCs =
    HP.class_ mdc_chip_set


chipSetActionCs :: HH.Attribute r i
chipSetActionCs =
    HP.class_ mdc_chip_set____action


gridRole :: HH.Attribute r i
gridRole =
    HH.Attributes.attribute "role" "grid"


chip :: Chip r i -> Html r i
chip (Chip ((Chip.Config { additionalAttributes }) as config_) label) =
    HH.div [ HP.class_ mdc_touch_target_wrapper ]
        [ HH.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , interactionHandler config_
                ]
                <> additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt label
                ]
            )
        ]


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


buttonRole :: HH.Attribute r i
buttonRole =
    HH.Attributes.attribute "role" "button"


rowRole :: Maybe (HH.Attribute r i)
rowRole =
    Just (HH.Attributes.attribute "role" "row")


gridcellRole :: HH.Attribute r i
gridcellRole =
    HH.Attributes.attribute "role" "gridcell"


interactionHandler :: Chip.Config r i -> Maybe (HH.Attribute r i)
interactionHandler (Chip.Config { onClick }) =
    Maybe.map (HH.Events.on "MDCChip:interaction" << Decode.succeed) onClick


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (HH.div [ HP.class_ mdc_chip__ripple ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { icon }) =
    Maybe.map
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
