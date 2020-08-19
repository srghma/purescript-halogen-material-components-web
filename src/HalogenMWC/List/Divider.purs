module HalogenMWC.Array.Divider
    ( Config, config
    , setPadded
    , setInset
    , setAttributes
    , listItem
    , group
    ) where

{-| Arrays are continuous, vertical indexes of text or images.

This module concerns a list's divider elements. If you are looking for the list
container, refer to [Material.Array](Material-Array), and if you are looking for
the list item, refer to [Material.Array.Item](Material-Array-Item).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Array Item Divider](#list-item-divider)
  - [Array Group Divider](#list-group-divider)


# Resources

  - [Demo: Arrays](https://aforemny.github.io/material-components-web-elm/#lists)
  - [Material Design Guidelines: Arrays](https://material.io/design/components/lists.html)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list#sass-mixins)


# Basic Usage

    import Material.Array as Array
    import Material.Array.Divider as ArrayDivider
    import Material.Array.Item as ArrayItem

    main =
        Array.list Array.config
            [ ArrayItem.listItem ArrayItem.config
                [ text "Line item" ]
            , ArrayDivider.listItem ArrayDivider.config
            , ArrayItem.listItem ArrayItem.config
                [ text "Line item" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setPadded
@docs setInset
@docs setAttributes


# Array Item Divider

Array items may be seperated by a divider. The divider may optionally be _inset_
so that it does not intersect the list item's graphic, or _padded_ so that it
does not intersect the list item's meta.

    Array.list Array.config
        [ ArrayItem.listItem ArrayItem.config
            [ text "Array item" ]
        , ArrayDivider.listItem ArrayDivider.config
        , ArrayItem.listItem ArrayItem.config
            [ text "Array item" ]
        ]

@docs listItem


### Array Group Divider

Multiple lists within a group may be visually seperated by a list group divider.

    Array.group []
        [ Array.list Array.config
            [ ArrayItem.listItem ArrayItem.config [ text "Folder" ]
            , ArrayItem.listItem ArrayItem.config [ text "Folder" ]
            ]
        , ArrayDivider.group []
        , Array.list Array.config
            [ ArrayItem.listItem ArrayItem.config [ text "File" ]
            , ArrayItem.listItem ArrayItem.config [ text "File" ]
            ]
        ]

@docs group

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import Html.Attributes (class)
import Material.Array.Item (ArrayItem)
import Material.Array.Item.Internal as ArrayItem


{-| Configuration of a list item divider
-}
type Config r i
    = Config
        { inset :: Boolean
        , padded :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of a list item divider
-}
config :: Config r i
config =
    Config
        { inset = False
        , padded = False
        , additionalAttributes = []
        }


{-| Specify whether a list divider should be _inset_

Insert list item dividers to not intersect a list item's meta.

-}
setInset :: Boolean -> Config r i -> Config r i
setInset inset (Config config_) =
    Config { config_ | inset = inset }


{-| Specify whether a list divider should be _padded_

Padded list item dividers do not intersect a list item's avatar.

-}
setPadded :: Boolean -> Config r i -> Config r i
setPadded padded (Config config_) =
    Config { config_ | padded = padded }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Array item divider view function
-}
listItem :: Config r i -> ArrayItem r i
listItem ((Config { additionalAttributes }) as config_) =
    ArrayItem.ArrayItemDivider <|
        Html.li
            (Array.filterMap identity
                [ listDividerCs
                , separatorRoleAttr
                , insetCs config_
                , paddedCs config_
                ]
                ++ additionalAttributes
            )
            []


listDividerCs :: Maybe (Html.Attribute r i)
listDividerCs =
    Just (class "mdc-list-divider")


separatorRoleAttr :: Maybe (Html.Attribute r i)
separatorRoleAttr =
    Just (Html.Attributes.attribute "role" "separator")


insetCs :: Config r i -> Maybe (Html.Attribute r i)
insetCs (Config { inset }) =
    if inset then
        Just (class "mdc-list-divider--inset")

    else
        Nothing


paddedCs :: Config r i -> Maybe (Html.Attribute r i)
paddedCs (Config { padded }) =
    if padded then
        Just (class "mdc-list-divider--padded")

    else
        Nothing


{-| Array group divider view function
-}
group :: Array (IProp r i) -> Html r i
group additionalAttributes =
    Html.hr (Array.filterMap identity [ listDividerCs ] ++ additionalAttributes) []
