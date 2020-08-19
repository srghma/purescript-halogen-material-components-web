module HalogenMWC.ChipSet.Filter (chipSet) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Chip.Filter.Internal as Chip
import Svg
import Svg.Attributes



chipSet :: Array (IProp r i) -> Array (Chip r i) -> Html r i
chipSet additionalAttributes chips =
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetFilterCs :: gridRole :: additionalAttributes)
        (Array.map chip chips)


chip :: Chip r i -> Html r i
chip (Chip ((Chip.Config { additionalAttributes }) as config_) label) =
    Html.div [ HP.class_ "mdc-touch-target-wrapper" ]
        [ Html.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , selectedProp config_
                , interactionHandler config_
                ]
                ++ additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , checkmarkElt
                , primaryActionElt label
                ]
            )
        ]


chipSetCs :: Html.Attribute r i
chipSetCs =
    HP.class_ "mdc-chip-set"


chipSetFilterCs :: Html.Attribute r i
chipSetFilterCs =
    HP.class_ "mdc-chip-set--filter"


gridRole :: Html.Attribute r i
gridRole =
    Html.Attributes.attribute "role" "grid"


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


selectedProp :: Chip.Config r i -> Maybe (Html.Attribute r i)
selectedProp (Chip.Config { selected }) =
    Just (Html.Attributes.property "selected" (Encode.bool selected))


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
interactionHandler (Chip.Config { onChange }) =
    Maybe.map (Html.Events.on "MDCChip:interaction" << Decode.succeed) onChange


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ HP.class_ "mdc-chip__ripple" ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { icon, selected }) =
    Maybe.map
        (\iconName ->
            Html.i
                [ HP.class_ "material-icons"
                , HP.class_ "mdc-chip__icon mdc-chip__icon--leading"
                ]
                [ text iconName ]
        )
        icon


checkmarkElt :: Maybe (Html r i)
checkmarkElt =
    Just
        (Html.div [ HP.class_ "mdc-chip__checkmark" ]
            [ Svg.svg
                [ Svg.Attributes.class_ "mdc-chip__checkmark-svg"
                , Svg.Attributes.viewBox "-2 -3 30 30"
                ]
                [ Svg.path
                    [ Svg.Attributes.class_ "mdc-chip__checkmark-path"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.d "M1.73,12.91 8.1,19.28 22.79,4.59"
                    ]
                    []
                ]
            ]
        )


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
