module HalogenMWC.Chip.Filter
    ( Config, config
    
    
    
    
    , chip, Chip
    ) where

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
  - [Filter Chips](#filter-chips)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import HalogenMWC.Chip.Filter as FilterChip
    import HalogenMWC.ChipSet.Filter as FilterChipSet

    data Msg
        = ChipClicked String

    main =
        FilterChipSet.chipSet []
            [ FilterChip.chip
                (FilterChip.config
                    |> FilterChip.setSelected True
                    |> FilterChip.setOnChange
                        (ChipClicked "Tops")
                )
                "Tops"
            , FilterChip.chip
                (FilterChip.config
                    |> FilterChip.setOnChange
                        (ChipClicked "Shoes")
                )
                "Shoes"
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnChange
@docs setIcon
@docs setSelected
@docs setAttributes


# Filter Chips

@docs chip, Chip

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Filter.Internal (Chip(..), Config(..))



type Config r i =
    Material.Chip.Filter.Internal.Config r i



config :: Config r i
config =
    Config
        { selected = False
        , icon = Nothing
        , onChange = Nothing
        , additionalAttributes = []
        }



setSelected :: Boolean -> Config r i -> Config r i
setSelected selected (Config config_) =
    Config { config_ | selected = selected }



setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



setOnChange :: r i -> Config r i -> Config r i
setOnChange onChange (Config config_) =
    Config { config_ | onChange = Just onChange }



data Chip r i =
    Material.Chip.Filter.Internal.Chip r i



chip :: Config r i -> String -> Chip r i
chip =
    Chip
