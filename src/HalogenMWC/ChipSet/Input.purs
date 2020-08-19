module HalogenMWC.ChipSet.Input (chipSet) where

{-| Chips are compact elements that allow users to enter information, select a
choice, filter content, or trigger an action.

Input chips are a variant of chips which enable user input by converting text
into chips.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Input Chip Set](#input-chip-set)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import HalogenMWC.Chip.Input as InputChip
    import HalogenMWC.ChipSet.Input as InputChipSet

    data Msg
        = ChipSelected String

    main =
        InputChipSet.chipSet []
            [ InputChip.chip InputChip.config "Chip One"
            , InputChip.chip InputChip.config "Chip Two"
            ]


# Input Chip Set

@docs chipSet

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import Html.Events
import Html.Keyed
import Json.Decode as Decode
import Json.Encode as Encode
import HalogenMWC.Chip.Input.Internal as Chip


{-| Input chip set view function
-}
chipSet :: Array (IProp r i) -> Array ( String, Chip r i ) -> Html r i
chipSet additionalAttributes keyedChips =
    Html.Keyed.node "mdc-chip-set"
        (chipSetCs :: chipSetInputCs :: gridRole :: additionalAttributes)
        (Array.map (Tuple.mapSecond chip) keyedChips)


chip :: Chip r i -> Html r i
chip (Chip ((Chip.Config { additionalAttributes }) as config_) label) =
    Html.div [ class "mdc-touch-target-wrapper" ]
        [ Html.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , removalHandler config_
                ]
                ++ additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt label
                , trailingIconElt config_
                ]
            )
        ]


chipSetCs :: Html.Attribute r i
chipSetCs =
    class "mdc-chip-set"


chipSetInputCs :: Html.Attribute r i
chipSetInputCs =
    class "mdc-chip-set--input"


gridRole :: Html.Attribute r i
gridRole =
    Html.Attributes.attribute "role" "grid"


chipCs :: Maybe (Html.Attribute r i)
chipCs =
    Just (class "mdc-chip")


chipTextCs :: Html.Attribute r i
chipTextCs =
    class "mdc-chip__text"


chipTouchCs :: Maybe (Html.Attribute r i)
chipTouchCs =
    Just (class "mdc-chip--touch")


chipPrimaryActionCs :: Html.Attribute r i
chipPrimaryActionCs =
    class "mdc-chip__primary-action"


tabIndexProp :: Int -> Html.Attribute r i
tabIndexProp tabIndex =
    Html.Attributes.property "tabIndex" (Encode.int tabIndex)


buttonRole :: Html.Attribute r i
buttonRole =
    Html.Attributes.attribute "role" "button"


rowRole :: Maybe (Html.Attribute r i)
rowRole =
    Just (Html.Attributes.attribute "role" "row")


gridcellRole :: Html.Attribute r i
gridcellRole =
    Html.Attributes.attribute "role" "gridcell"


removalHandler :: Chip.Config r i -> Maybe (Html.Attribute r i)
removalHandler (Chip.Config { onDelete }) =
    Maybe.map (Html.Events.on "MDCChip:removal" << Decode.succeed) onDelete


rippleElt :: Maybe (Html r i)
rippleElt =
    Just (Html.div [ class "mdc-chip__ripple" ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { leadingIcon }) =
    Maybe.map
        (\iconName ->
            Html.i [ class "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        leadingIcon


primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just <|
        Html.span [ chipPrimaryActionCs, gridcellRole, tabIndexProp -1 ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (Html.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (Html.div [ class "mdc-chip__touch" ] [])


trailingIconElt :: Chip.Config r i -> Maybe (Html r i)
trailingIconElt (Chip.Config { trailingIcon, onDelete }) =
    if onDelete /= Nothing then
        Just <|
            Html.i
                [ class "material-icons mdc-chip__icon mdc-chip__icon--trailing"
                , tabIndexProp -1
                , buttonRole
                ]
                [ text (Maybe.withDefault "cancel" trailingIcon) ]

    else
        Nothing
