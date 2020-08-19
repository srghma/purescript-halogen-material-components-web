module HalogenMWC.Chip.Action
    ( Config, config
    , setOnClick
    , setIcon
    , setAttributes
    , chip, Chip
    ) where

{-| Action chips offer actions related to primary content. They should appear
dynamically and contextually in a UI.

An alternative to action chips are [buttons](Material-Button), which should
appear persistently and consistently.


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Action Chips](#action-chips)


# Resources

  - [Demo: Chips](https://aforemny.github.io/material-components-web-elm/#chips)
  - [Material Design Guidelines: Chips](https://material.io/go/design-chips)
  - [MDC Web: Chips](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-chips#sass-mixins)


# Basic Usage

    import HalogenMWC.Chip.Action as ActionChip
    import HalogenMWC.ChipSet.Action as ActionChipSet

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


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setIcon
@docs setAttributes


# Action Chips

@docs chip, Chip

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Chip.Action.Internal (Chip(..), Config(..))


{-| Configuration of an action chip
-}
type Config r i =
    Material.Chip.Action.Internal.Config r i


{-| Default configuration of an action chip
-}
config :: Config r i
config =
    Config
        { icon = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        }


{-| Specify whether the chip displays an icon
-}
setIcon :: Maybe String -> Config r i -> Config r i
setIcon icon (Config config_) =
    Config { config_ | icon = icon }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user clicks on a chip
-}
setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config { config_ | onClick = Just onClick }


{-| Action chip type
-}
data Chip r i =
    Material.Chip.Action.Internal.Chip r i


{-| Action chip view function
-}
chip :: Config r i -> String -> Chip r i
chip =
    Chip
