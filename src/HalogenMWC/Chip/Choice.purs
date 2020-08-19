module HalogenMWC.Chip.Choice
    ( Config, config
    
    
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

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Choice.Internal (Chip(..), Config(..))



type Config r i =
    Material.Chip.Choice.Internal.Config r i



config :: Config r i
config =
    Config
        { icon = Nothing
        , additionalAttributes = []
        }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



data Chip a r i =
    Material.Chip.Choice.Internal.Chip a r i



chip :: Config r i -> a -> Chip a r i
chip =
    Chip
