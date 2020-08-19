module HalogenMWC.Select.Item
    ( Config, config
    
    
    , SelectItem, selectItem
    ) where

{-| Select provides a single-option select menus.

This module concerns the select items. If you are looking for the select container,
refer to [Material.Select](Material-Select).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Select Item](#select-item)
  - [Disabled Select Item](#disabled-select-item)


# Resources

  - [Demo: Selects](https://aforemny.github.io/material-components-web-elm/#select)
  - [Material Design Guidelines: Text Fields](https://material.io/go/design-text-fields)
  - [MDC Web: Select](https://github.com/material-components/material-components-web/tree/master/packages/mdc-select)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-select#sass-mixins)


# Basic Usage

    import HalogenMWC.Select as Select
    import HalogenMWC.Select.Item as SelectItem

    data Msg
        = ValueChanged String

    main =
        Select.filled
            (Select.config
                |> Select.setLabel (Just "Fruit")
                |> Select.setSelected (Just "")
                |> Select.setOnChange ValueChanged
            )
            (SelectItem.selectItem
                (SelectItem.config { value = "" })
                [ text "" ]
            )
            [ SelectItem.selectItem
                (SelectItem.config { value = "Apple" })
                [ text "Apple" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setDisabled
@docs setAttributes


# Select Item

@docs SelectItem, selectItem


# Disabled Select Item

Select items may be disabled by setting their `setDisabled` configuration option
to `True`.

    SelectItem.selectItem
        (SelectItem.config { value = "Apple" }
            |> SelectItem.setDisabled True
        )
        [ text "Apple" ]

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.Select.Item.Internal (Config(..), SelectItem(..))


{-| Configuration of a select item
-}
data Config a r i =
    Material.Select.Item.Internal.Config a r i


{-| Default configuration of a select item
-}
config :: { value :: a } -> Config a r i
config { value } =
    Config
        { value = value
        , disabled = False
        , additionalAttributes = []
        }


{-| Specify whether a select item should be disabled

Disabled select items cannot be interacted with and have not visual interaction
effect.

-}
setDisabled :: Boolean -> Config a r i -> Config a r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config a r i -> Config a r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Select item type
-}
data SelectItem a r i =
    Material.Select.Item.Internal.SelectItem a r i


{-| Select item constructor
-}
selectItem :: Config a r i -> Array (Html r i) -> SelectItem a r i
selectItem =
    SelectItem
