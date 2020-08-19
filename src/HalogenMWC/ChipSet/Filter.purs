module HalogenMWC.ChipSet.Filter (chipSet)

{-| Chips are compact elements that allow users to enter information, select a
choice, filter content, or trigger an action.

Filter chips are a variant of chips which allow multiple selection from a set
of options. When a filter chip is selected, a checkmark appears as the leading
icon. If the chip already has a leading icon, the checkmark replaces it.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Filter Chip Set](#filter-chip-set)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import Material.Chip.Filter as FilterChip
    import Material.ChipSet.Filter as FilterChipSet

    data Msg
        = ChipClicked String

    main =
        FilterChipSet.chipSet []
            [ FilterChip.chip
                (FilterChip.config
                    |> FilterChip.setSelected True
                    |> FilterChip.setOnClick
                        (ChipClicked "Tops")
                )
                "Tops"
            , FilterChip.chip
                (FilterChip.config
                    |> FilterChip.setOnClick
                        (ChipClicked "Shoes")
                )
                "Shoes"
            ]


# Filter Chip Set

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
import Json.Encode as Encode
import Material.Chip.Filter.Internal as Chip (Chip(..))
import Svg
import Svg.Attributes


{-| Filter chip set view function
-}
chipSet :: Array (IProp r i) -> Array (Chip r i) -> Html r i
chipSet additionalAttributes chips =
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetFilterCs :: gridRole :: additionalAttributes)
        (Array.map chip chips)


chip :: Chip r i -> Html r i
chip (Chip ((Chip.Config { additionalAttributes }) as config_) label) =
    Html.div [ class "mdc-touch-target-wrapper" ]
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
    class "mdc-chip-set"


chipSetFilterCs :: Html.Attribute r i
chipSetFilterCs =
    class "mdc-chip-set--filter"


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
    Just (Html.div [ class "mdc-chip__ripple" ] [])


leadingIconElt :: Chip.Config r i -> Maybe (Html r i)
leadingIconElt (Chip.Config { icon, selected }) =
    Maybe.map
        (\iconName ->
            Html.i
                [ class "material-icons"
                , class "mdc-chip__icon mdc-chip__icon--leading"
                ]
                [ text iconName ]
        )
        icon


checkmarkElt :: Maybe (Html r i)
checkmarkElt =
    Just
        (Html.div [ class "mdc-chip__checkmark" ]
            [ Svg.svg
                [ Svg.Attributes.class "mdc-chip__checkmark-svg"
                , Svg.Attributes.viewBox "-2 -3 30 30"
                ]
                [ Svg.path
                    [ Svg.Attributes.class "mdc-chip__checkmark-path"
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
    Just <|
        Html.span [ chipPrimaryActionCs, gridcellRole ]
            (Array.filterMap identity [ textElt label, touchElt ])


textElt :: String -> Maybe (Html r i)
textElt label =
    Just (Html.span [ chipTextCs, buttonRole ] [ text label ])


touchElt :: Maybe (Html r i)
touchElt =
    Just (Html.div [ class "mdc-chip__touch" ] [])
