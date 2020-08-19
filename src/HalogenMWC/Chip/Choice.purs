module HalogenMWC.Chip.Choice
    ( Config, config
    , setIcon
    , setAttributes
    , chip, Chip
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
  - [Choice Chips](#choice-chips)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import Material.Chip.Choice as ChoiceChip
    import Material.ChipSet.Choice as ChoiceChipSet

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
                |> ChocieChipSet.setOnClick ColorChanged
            )
            [ ChoiceChip.chip ChoiceChip.config Red
            , ChoiceChip.chip ChoiceChip.config Blue
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setIcon
@docs setAttributes


# Choice Chips

@docs chip, Chip

-}

import Html
import Material.Chip.Choice.Internal (Chip(..), Config(..))


{-| Configuration of a choice chip
-}
type Config r i =
    Material.Chip.Choice.Internal.Config msg


{-| Default configuration of a choice chip
-}
config :: Config msg
config =
    Config
        { icon = Nothing
        , additionalAttributes = []
        }


{-| Specify whether the chip displays an icon
-}
setIcon :: Maybe String -> Config msg -> Config msg
setIcon icon (Config config_) =
    Config { config_ | icon = icon }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Choice chip type
-}
data Chip a msg =
    Material.Chip.Choice.Internal.Chip a msg


{-| Choice chip view function
-}
chip :: Config msg -> a -> Chip a msg
chip =
    Chip
