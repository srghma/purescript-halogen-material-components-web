module HalogenMWC.Array.Item
    ( Config, config
    
    
    
    
    
    
    , ArrayItem, listItem
    , graphic
    , meta
    , text
    , Selection, selected
    , activated
    ) where

{-| Arrays are continuous, vertical indexes of text or images.

This module concerns the list items. If you are looking for the list container,
refer to [Material.Array](Material-Array).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Array Item](#list-item)
  - [Array Item with Graphic](#list-item-with-graphic)
  - [Array Item with Meta](#list-item-with-meta)
  - [Two-Line Array Item](#two-line-list-item)
  - [Disabled Array Item](#disabled-list-item)
  - [Selected Array Item](#selected-list-item)
  - [Activated Array Item](#activated-list-item)
  - [Link Array Item](#link-list-item)
  - [Array Item Divider](#list-item-divider)


# Resources

  - [Demo: Arrays](https://aforemny.github.io/material-components-web-elm/#lists)
  - [Material Design Guidelines: Arrays](https://material.io/design/components/lists.html)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list#sass-mixins)


# Basic Usage

    import HalogenMWC.Array as Array
    import HalogenMWC.Array.Item as ArrayItem

    main =
        Array.list Array.config
            [ ArrayItem.listItem ArrayItem.config
                [ text "Line item" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setOnClick
@docs setDisabled
@docs setSelected
@docs setHref
@docs setTarget
@docs setAttributes


# Array Item

@docs ArrayItem, listItem


# Array Item with Graphic

In addition to their text, list itemss may optionally contain a starting tile
referred to as _graphic_.

Common examples for graphics are icons and images, avatar images and selection
controls such as checkboxes.

    ArrayItem.listItem ArrayItem.config
        [ ArrayItem.graphic [] [ Icon.icon Icon.config "star" ]
        , text "Array item"
        ]

@docs graphic


# Array Item with Meta

In addition to their text child, list items may optionally contain a starting
tile referred to as _meta_.

Common examples for metas are text, icons and images and selection controls.

    ArrayItem.listItem ArrayItem.config
        [ text "Array item"
        , ArrayItem.meta [] [ Icon.icon Icon.config "star" ]
        ]

@docs meta


# Two-Line Array Item

Array items may be two-line list items by using `text`.

    ArrayItem.listItem ArrayItem.config
        [ ArrayItem.text []
            { primary = [ text "First line" ]
            , secondary = [ text "Second line" ]
            }
        ]

@docs text


# Disabled Array Item

Array items may be disabled by setting their `setDisabled` configuration option
to `True`.

    ArrayItem.listItem
        (ArrayItem.config |> ArrayItem.setDisabled True)
        [ text "Array item" ]


### Selected Array Item

Array items may be selected by setting their `setSelected` configuration option
to a value of `Selection`.

A list item that may change its selection state within the current page, should
be selected rather than activated.

As a rule of thumb, a navigation list item should be activated, while any other
list item should be selected.

    ArrayItem.listItem
        (ArrayItem.config
            |> ArrayItem.setSelected (Just ArrayItem.selected)
        )
        [ text "Array item" ]

@docs Selection, selected


### Activated Array Item

Array items may be activated by setting their `setSelected` configuration option
to a value of `Selection`.

A list item that may not change its state within the current page should be
activated rather than selected.

As a rule of thumb, a navigation list item should be activated, while any other
list item should be selected.

    ArrayItem.listItem
        (ArrayItem.config
            |> ArrayItem.setSelected (Just ArrayItem.activated)
        )
        [ text "Array item" ]

@docs activated


## Link Array Item

Array items may using the `setHref` configuration option in which case the list
item essentially behaves like a HTML anchor element. You may specify the
configuration option `setTarget` as well.

    ArrayItem.listItem
        (ArrayItem.config
            |> ArrayItem.setHref (Just "https://elm-lang.org")
        )
        [ text "Elm programming language" ]

Note that link list items cannot be disabled.

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.Array.Item.Internal (Config(..), ArrayItem(..), Selection(..))


{-| Configuration of a list item
-}
type Config r i =
    Material.Array.Item.Internal.Config r i


{-| Default configuration of a list item
-}
config :: Config r i
config =
    Config
        { disabled = False
        , selection = Nothing
        , href = Nothing
        , target = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        , node = Html.text ""
        }


{-| Specify whether a list item should be disabled

Disabled list items cannot be interacted with and have not visual interaction
effect.

-}
setDisabled :: Boolean -> Config r i -> Config r i
setDisabled disabled (Config config_) =
    Config { config_ | disabled = disabled }


{-| Selection of a list item

A list item may be either in selected or in activated selection state.

-}
data Selection =
    Material.Array.Item.Internal.Selection


{-| Selected selection state
-}
selected :: Selection
selected =
    Selected


{-| Activated selection state
-}
activated :: Selection
activated =
    Activated


{-| Specify whether a list item is selected

A selected list item may be either _selected_ or _activated_. A list item that
may change its selection state within the current page, should be selected. A
list item that may not change its state within the current page should be
activated.

As a rule of thumb, a navigation list item should be activated, while any other
list item should be selected.

-}
setSelected :: Maybe Selection -> Config r i -> Config r i
setSelected selection (Config config_) =
    Config { config_ | selection = selection }


{-| Specify whether a list item is a _link list item_

Link list items essentially behave like a HTML5 anchor element.

-}
setHref :: Maybe String -> Config r i -> Config r i
setHref href (Config config_) =
    Config { config_ | href = href }


{-| Specify a link list item's HTML5 target attribute

Note that non-link list items ignore this configuration option.

-}
setTarget :: Maybe String -> Config r i -> Config r i
setTarget target (Config config_) =
    Config { config_ | target = target }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config
        { config_ | additionalAttributes = additionalAttributes }


{-| Specify a message when the user interacts with the list item
-}
setOnClick :: r i -> Config r i -> Config r i
setOnClick onClick (Config config_) =
    Config
        { config_ | onClick = Just onClick }


{-| Array item type

Array items can only be rendered within a [list container](Material-Array).

-}
data ArrayItem r i =
    Material.Array.Item.Internal.ArrayItem r i


{-| Array item constructor
-}
listItem :: Config r i -> Array (Html r i) -> ArrayItem r i
listItem (Config ({ additionalAttributes, href } as config_)) nodes =
    ArrayItem (Config { config_ | node = listItemView (Config config_) nodes })


listItemView :: Config r i -> Array (Html r i) -> Html r i
listItemView ((Config { additionalAttributes, href }) as config_) nodes =
    (\attributes ->
        if href /= Nothing then
            Html.node "mdc-list-item" [] [ Html.a attributes nodes ]

        else
            Html.node "mdc-list-item" attributes nodes
    ) where
        (Array.filterMap identity
            [ listItemCs
            , hrefAttr config_
            , targetAttr config_
            , disabledCs config_
            , selectedCs config_
            , activatedCs config_
            , ariaSelectedAttr config_
            ]
            ++ additionalAttributes
        )


listItemCs :: Maybe (Html.Attribute r i)
listItemCs =
    Just (class "mdc-list-item")


disabledCs :: Config r i -> Maybe (Html.Attribute r i)
disabledCs (Config { disabled }) =
    if disabled then
        Just (class "mdc-list-item--disabled")

    else
        Nothing


selectedCs :: Config r i -> Maybe (Html.Attribute r i)
selectedCs (Config { selection }) =
    if selection == Just Selected then
        Just (class "mdc-list-item--selected")

    else
        Nothing


activatedCs :: Config r i -> Maybe (Html.Attribute r i)
activatedCs (Config { selection }) =
    if selection == Just Activated then
        Just (class "mdc-list-item--activated")

    else
        Nothing


ariaSelectedAttr :: Config r i -> Maybe (Html.Attribute r i)
ariaSelectedAttr (Config { selection }) =
    if selection /= Nothing then
        Just (Html.Attributes.attribute "aria-selected" "true")

    else
        Nothing


hrefAttr :: Config r i -> Maybe (Html.Attribute r i)
hrefAttr (Config { href }) =
    Maybe.map Html.Attributes.href href


targetAttr :: Config r i -> Maybe (Html.Attribute r i)
targetAttr (Config { href, target }) =
    if href /= Nothing then
        Maybe.map Html.Attributes.target target

    else
        Nothing


{-| Two-line list item's text
-}
text :
    Array (IProp r i)
    ->
        { primary :: Array (Html r i)
        , secondary :: Array (Html r i)
        }
    -> Html r i
text additionalAttributes { primary, secondary } =
    Html.div (class "mdc-list-item__text" :: additionalAttributes)
        [ primaryText [] primary
        , secondaryText [] secondary
        ]


primaryText :: Array (IProp r i) -> Array (Html r i) -> Html r i
primaryText additionalAttributes nodes =
    Html.div (class "mdc-list-item__primary-text" :: additionalAttributes) nodes


secondaryText :: Array (IProp r i) -> Array (Html r i) -> Html r i
secondaryText additionalAttributes nodes =
    Html.div (class "mdc-list-item__secondary-text" :: additionalAttributes) nodes


{-| A list item's graphic tile
-}
graphic :: Array (IProp r i) -> Array (Html r i) -> Html r i
graphic additionalAttributes nodes =
    Html.div (class "mdc-list-item__graphic" :: additionalAttributes) nodes


{-| A list item's meta tile
-}
meta :: Array (IProp r i) -> Array (Html r i) -> Html r i
meta additionalAttributes nodes =
    Html.div (class "mdc-list-item__meta" :: additionalAttributes) nodes
