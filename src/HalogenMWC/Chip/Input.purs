module HalogenMWC.Chip.Input
    ( Config, config
    , setOnClick
    , setOnDelete
    , setLeadingIcon
    , setTrailingIcon
    , setAttributes
    , chip, Chip
    ) where

{-| Chips are compact elements that allow users to enter information, select a
choice, filter content, or trigger an action.

Input chips are a variant of chips which enable user input by converting text
into chips.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Input Chips](#input-chips)


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


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setOnDelete
@docs setLeadingIcon
@docs setTrailingIcon
@docs setAttributes


# Input Chips

@docs chip, Chip

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Input.Internal (Chip(..), Config(..))


{-| Configuration of an input chip
-}
type Config r i =
    Material.Chip.Input.Internal.Config r i


{-| Default configuration of an input chip
-}
config :: Config r i
config =
    Config
        { leadingIcon = Nothing
        , trailingIcon = Nothing
        , additionalAttributes = []
        , onDelete = Nothing
        , onClick = Nothing
        }


{-| Specify whether an input chip displays a leading icon
-}
setLeadingIcon :: Maybe String -> Config r i -> Config r i
setLeadingIcon leadingIcon (Config config_) =
    Config { config_ | leadingIcon = leadingIcon }


{-| Specify whether an input chip displays a trailing icon
-}
setTrailingIcon :: Maybe String -> Config r i -> Config r i
setTrailingIcon trailingIcon (Config config_) =
    Config { config_ | trailingIcon = trailingIcon }


{-| Specify additonal attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user clicks on a chip's trailing icon
-}
setOnDelete :: r i -> Config r i -> Config r i
setOnDelete onDelete (Config config_) =
    Config { config_ | onDelete = Just onDelete }


{-| Specify a message when the user clicks on a chip
-}
setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }


{-| Input chip type
-}
data Chip r i =
    Material.Chip.Input.Internal.Chip r i


{-| Input chip view function
-}
chip :: Config r i -> String -> Chip r i
chip =
    Chip
