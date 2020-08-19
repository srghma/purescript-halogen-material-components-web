module HalogenMWC.ChipSet.Choice
    ( Config, config
    
    
    
    , chipSet
    ) where

{-| Chips are compact elements that allow users to enter information, select a
choice, filter content, or trigger an action.

Choice chips are a variant of chips which allow single selection from a set of
options.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Choice Chip Set](#choice-chip-set)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import HalogenMWC.Chip.Choice as ChoiceChip
    import HalogenMWC.ChipSet.Choice as ChoiceChipSet

    data Color
        = Red
        | Blue

    data Msg
        = ColorChanged Color

    main =
        ChoiceChipSet.chipSet
            (ChoiceChipSet.config
                { toLabel =
                    \color ->
                        case color of
                            Red ->
                                "Red"

                            Blue ->
                                "Blue"
                }
                |> ChoiceChipSet.setSelected (Just Red)
                |> ChocieChipSet.setOnChange ColorChanged
            )
            [ ChoiceChip.chip ChoiceChip.config Red
            , ChoiceChip.chip ChoiceChip.config Blue
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setSelected
@docs setOnChange
@docs setAttributes


# Choice Chip Set

@docs chipSet

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA




import HalogenMWC.Chip.Choice.Internal as Chip


{-| Configuration of a choice chip set
-}
data Config a r i
    =
        { selected :: Maybe a
        , onChange :: Maybe (a -> r i)
        , toLabel :: a -> String
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of a choice chip set
-}
config :: { toLabel :: a -> String } -> Config a r i
config { toLabel } =
    Config
        { selected = Nothing
        , onChange = Nothing
        , toLabel = toLabel
        , additionalAttributes = []
        }


{-| Specify which chip is selected
-}
setSelected :: Maybe a -> Config a r i -> Config a r i
setSelected selected (Config config_) =
    Config { config_ | selected = selected }


{-| Specify a message when the user clicks on a chip
-}
setOnChange :: (a -> r i) -> Config a r i -> Config a r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config a r i -> Config a r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Choice chip set view function
-}
chipSet :: Config a r i -> Array (Chip a r i) -> Html r i
chipSet ((Config { selected, onChange, toLabel, additionalAttributes }) as config_) chips =
    Html.node "mdc-chip-set"
        (chipSetCs :: chipSetChoiceCs :: gridRole :: additionalAttributes)
        (Array.map (chip selected onChange toLabel) chips)


chip :: Maybe a -> Maybe (a -> r i) -> (a -> String) -> Chip a r i -> Html r i
chip selected onChange toLabel (Chip ((Chip.Config { additionalAttributes }) as config_) value) =
    Html.div [ class "mdc-touch-target-wrapper" ]
        [ Html.node "mdc-chip"
            (Array.filterMap identity
                [ chipCs
                , chipTouchCs
                , rowRole
                , selectedProp (Just value == selected)
                , interactionHandler (Maybe.map ((|>) value) onChange)
                ]
                ++ additionalAttributes
            )
            (Array.filterMap identity
                [ rippleElt
                , leadingIconElt config_
                , primaryActionElt (toLabel value)
                ]
            )
        ]


chipSetCs :: Html.Attribute r i
chipSetCs =
    class "mdc-chip-set"


chipSetChoiceCs :: Html.Attribute r i
chipSetChoiceCs =
    class "mdc-chip-set--choice"


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


selectedProp :: Boolean -> Maybe (Html.Attribute r i)
selectedProp selected =
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


interactionHandler :: Maybe r i -> Maybe (Html.Attribute r i)
interactionHandler r i =
    Maybe.map (Html.Events.on "MDCChip:interaction" << Decode.succeed) r i


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
