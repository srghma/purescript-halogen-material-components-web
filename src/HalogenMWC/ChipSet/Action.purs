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

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import Html.Events
import Json.Decode as Decode
import Material.Chip.Action.Internal as Chip (Chip(..))


{-| Chip set view function
-}
chipSet :: Array (IProp r i) -> Array (Chip r i) -> Html r i
chipSet additionalAttributes chips =
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetActionCs :: gridRole :: additionalAttributes)
        (Array.map chip chips)


chipSetCs :: Html.Attribute r i
chipSetCs =
    class "mdc-chip-set"


chipSetActionCs :: Html.Attribute r i
chipSetActionCs =
    class "mdc-chip-set--action"


gridRole :: Html.Attribute r i
gridRole =
    Html.Attributes.attribute "role" "grid"


chip :: Chip r i -> Html r i
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
    Just (Html.div [ class "mdc-chip__ripple" ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { icon }) =
    Maybe.map
        (\iconName ->
            Html.i [ class "material-icons mdc-chip__icon mdc-chip__icon--leading" ]
                [ text iconName ]
        )
        icon


primaryActionElt :: String -> Maybe (Html r i)
primaryActionElt label =
    Just <|
        Html.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (Html.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (Html.div [ class "mdc-chip__touch" ] [])
