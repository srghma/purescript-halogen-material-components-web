module HalogenMWC.ChipSet.Action (chipSet)

{-| Action chips offer actions related to primary content. They should appear
dynamically and contextually in a UI.

An alternative to action chips are [buttons](Material-Button), which should
appear persistently and consistently.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Action Chip Set](#action-chip-set)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import Material.Chip.Action as ActionChip
    import Material.ChipSet.Action as ActionChipSet

    data Msg
        = Clicked String

    main =
        ActionChipSet.chipSet []
            [ ActionChip.chip
                (ActionChip.config
                    |> ActionChip.setOnClick Clicked "Chip One"
                )
                "Chip One"
            , ActionChip.chip ActionChip.config "Chip Two"
            ]


# Action Chip Set

@docs chipSet

-}

import Html (Html, text)
import Html.Attributes (class)
import Html.Events
import Json.Decode as Decode
import Material.Chip.Action.Internal as Chip (Chip(..))


{-| Chip set view function
-}
chipSet :: Array (IProp r i) -> Array (Chip msg) -> Html msg
chipSet additionalAttributes chips =
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetActionCs :: gridRole :: additionalAttributes)
        (Array.map chip chips)


chipSetCs :: Html.Attribute msg
chipSetCs =
    class "mdc-chip-set"


chipSetActionCs :: Html.Attribute msg
chipSetActionCs =
    class "mdc-chip-set--action"


gridRole :: Html.Attribute msg
gridRole =
    Html.Attributes.attribute "role" "grid"


chip :: Chip msg -> Html msg
chip (Chip ((Chip.Config { additionalAttributes }) as config_) label) =
    Html.div [ class "mdc-touch-target-wrapper" ]
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


chipCs :: Maybe (Html.Attribute msg)
chipCs =
    Just (class "mdc-chip")


chipTextCs :: Html.Attribute msg
chipTextCs =
    class "mdc-chip__text"


chipTouchCs :: Maybe (Html.Attribute msg)
chipTouchCs =
    Just (class "mdc-chip--touch")


chipPrimaryActionCs :: Html.Attribute msg
chipPrimaryActionCs =
    class "mdc-chip__primary-action"


buttonRole :: Html.Attribute msg
buttonRole =
    Html.Attributes.attribute "role" "button"


rowRole :: Maybe (Html.Attribute msg)
rowRole =
    Just (Html.Attributes.attribute "role" "row")


gridcellRole :: Html.Attribute msg
gridcellRole =
    Html.Attributes.attribute "role" "gridcell"


interactionHandler :: Chip.Config msg -> Maybe (Html.Attribute msg)
interactionHandler (Chip.Config { onClick }) =
    Maybe.map (Html.Events.on "MDCChip:interaction" << Decode.succeed) onClick


rippleElt :: Maybe (Html msg)
rippleElt =
    Just (Html.div [ class "mdc-chip__ripple" ] [])


leadingIconElt :: Chip.Config msg -> Maybe (Html msg)
leadingIconElt (Chip.Config { icon }) =
    Maybe.map
        (\iconName ->
            Html.i [ class "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        icon


primaryActionElt :: String -> Maybe (Html msg)
primaryActionElt label =
    Just <|
        Html.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html msg)
textElt label =
    Just (Html.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html msg)
touchElt =
    Just (Html.div [ class "mdc-chip__touch" ] [])
