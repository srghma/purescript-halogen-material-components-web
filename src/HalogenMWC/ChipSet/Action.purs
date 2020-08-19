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
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetActionCs :: gridRole :: additionalAttributes)
        (Array.map chip chips)


chipSetCs :: Html.Attribute r i
chipSetCs =
    HP.class_ "mdc-chip-set"


chipSetActionCs :: Html.Attribute r i
chipSetActionCs =
    HP.class_ "mdc-chip-set--action"


gridRole :: Html.Attribute r i
gridRole =
    Html.Attributes.attribute "role" "grid"


chip :: Chip r i -> Html r i
chip (Chip ((Chip.Config { additionalAttributes }) as config_) label) =
    Html.div [ HP.class_ "mdc-touch-target-wrapper" ]
        [ Html.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , interactionHandler config_
                ]
                ++ additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt label
                ]
            )
        ]


chipCs :: Maybe (Html.Attribute r i)
chipCs =
    Just (HP.class_ "mdc-chip")


chipTextCs :: Html.Attribute r i
chipTextCs =
    HP.class_ "mdc-chip__text"


chipTouchCs :: Maybe (Html.Attribute r i)
chipTouchCs =
    Just (HP.class_ "mdc-chip--touch")


chipPrimaryActionCs :: Html.Attribute r i
chipPrimaryActionCs =
    HP.class_ "mdc-chip__primary-action"


buttonRole :: Html.Attribute r i
buttonRole =
    Html.Attributes.attribute "role" "button"


rowRole :: Maybe (Html.Attribute r i)
rowRole =
    Just (Html.Attributes.attribute "role" "row")


gridcellRole :: Html.Attribute r i
gridcellRole =
    Html.Attributes.attribute "role" "gridcell"


interactionHandler :: Chip.Config r i -> Maybe (Html.Attribute r i)
interactionHandler (Chip.Config { onClick }) =
    Maybe.map (Html.Events.on "MDCChip:interaction" << Decode.succeed) onClick


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ HP.class_ "mdc-chip__ripple" ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { icon }) =
    Maybe.map
        (\iconName ->
            Html.i [ HP.class_ "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        icon


primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just $
        Html.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (Html.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (Html.div [ HP.class_ "mdc-chip__touch" ] [])
