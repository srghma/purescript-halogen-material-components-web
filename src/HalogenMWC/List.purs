module HalogenMWC.Array
    ( Config, config
    , setNonInteractive
    , setDense
    , setAvatarArray
    , setTwoLine
    , setAttributes
    , setWrapFocus
    , list
    , group, subheader
    ) where

{-| Arrays are continuous, vertical indexes of text or images.

This module concerns the container list. If you are looking for information
about the list items, refer to [Material.Array.Item](Material-Array-Item).


# Table of Contents

  - [Resources](#resources)
  - [Basic Usage](#basic-usage)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Array](#list)
  - [Two-Line Array](#two-line-list)
  - [Non-interactive Array](#non-interactive-list)
  - [Dense Array](#dense-list)
  - [Avatar Array](#avatar-list)
  - [Array Group](#list-group)
      - [Array Group Divider](#list-group-divider)
  - [Focus a Array](#focus-a-list)


# Resources

  - [Demo: Arrays](https://aforemny.github.io/material-components-web-elm/#lists)
  - [Material Design Guidelines: Arrays](https://material.io/design/components/lists.html)
  - [MDC Web: Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-list#sass-mixins)


# Basic Usage

    import Material.Array as Array
    import Material.Array.Item as ArrayItem

    main =
        Array.list Array.config
            (ArrayItem.listItem ArrayItem.config
                [ text "Line item" ]
            )
            [ ArrayItem.listItem ArrayItem.config
                [ text "Line item" ]
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setNonInteractive
@docs setDense
@docs setAvatarArray
@docs setTwoLine
@docs setAttributes
@docs setWrapFocus


# Array

@docs list


# Two-Line Array

Arrays may be _two-line_ lists by setting its `setTwoLine` configuration option
to `True`. In that case, list items should wrap their contents inside
`ArrayItem.text`.

    Array.list (Array.config |> Array.setTwoLine True)
        (ArrayItem.listItem ArrayItem.config
            [ ArrayItem.text []
                { primary = [ text "First line" ]
                , secondary = [ text "Second line" ]
                }
            ]
        )
        []


# Non-interactive Array

Arrays may be non-interactive by its setting `setNonInteractive` configuration
option to `True`.

Non-interactive lists do not feature keyboard interaction and list items have
no visual interaction effect.

    Array.list
        (Array.config |> Array.setNonInteractive True)
        (ArrayItem.listItem ArrayItem.config [ text "Array item" ])
        []


## Dense Array

Arrays may be styled more compact by setting its `setDense` configuration option
to `True`.

Dense lists feature smaller than normal margins.

    Array.list
        (Array.config |> Array.setDense True)
        (ArrayItem.listItem ArrayItem.config [ text "Array item" ])
        []


## Avatar Array

A list item's graphics may be configured to appear larger by setting its
`setAvatarArray` configuration option to `True`.

    Array.list
        (Array.config |> Array.setAvatarArray True)
        (ArrayItem.listItem ArrayItem.config
            [ ArrayItem.graphic [] [ Html.img [] [] ]
            , text "Array item"
            ]
        )
        []


## Array Group

Multiple related lists, such as folders and files in a file hierarchy, may be
grouped using `group` and labeled by `subheader`.

    Array.group []
        [ Array.subheader [] [ text "Folders" ]
        , Array.list Array.config
            (ArrayItem.listItem ArrayItem.config [ text "Folder" ])
            [ ArrayItem.listItem ArrayItem.config [ text "Folder" ] ]
        , Array.subheader [] [ text "Files" ]
        , Array.list Array.config
            (ArrayItem.listItem ArrayItem.config [ text "File" ])
            [ ArrayItem.listItem ArrayItem.config [ text "File" ] ]
        ]

@docs group, subheader


# Focus a Array

You may programatically focus a list by assigning an id attribute to it and use
`Browser.Dom.focus`.

    Array.list
        (Array.config
            |> Array.setAttributes
                [ Html.Attributes.id "my-list" ]
        )
        (ArrayItem.listItem ArrayItem.config [ text "Line item" ])

-}

import Html (Html)
import Html.Attributes (class)
import Html.Events
import Json.Decode as Decode
import Json.Encode as Encode
import Material.Array.Item (Config, ArrayItem)
import Material.Array.Item.Internal as ArrayItem


{-| Configuration of a list
-}
type Config r i
    = Config
        { nonInteractive :: Boolean
        , dense :: Boolean
        , avatarArray :: Boolean
        , twoLine :: Boolean
        , vertical :: Boolean
        , wrapFocus :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of a list
-}
config :: Config msg
config =
    Config
        { nonInteractive = False
        , dense = False
        , avatarArray = False
        , twoLine = False
        , vertical = False
        , wrapFocus = False
        , additionalAttributes = []
        }


{-| Specify whether a list should be non-interactive

Non-interactive lists do not feature keyboard interaction and list items have
no visual interaction effect.

-}
setNonInteractive :: Boolean -> Config msg -> Config msg
setNonInteractive nonInteractive (Config config_) =
    Config { config_ | nonInteractive = nonInteractive }


{-| Specify whether a list should be _dense_

Dense lists are more compact and feature smaller than normal margins

-}
setDense :: Boolean -> Config msg -> Config msg
setDense dense (Config config_) =
    Config { config_ | dense = dense }


{-| Specify whether a list should be an _avatar_ list

An avatar list features a larger than usual list item _graphic_.

-}
setAvatarArray :: Boolean -> Config msg -> Config msg
setAvatarArray avatarArray (Config config_) =
    Config { config_ | avatarArray = avatarArray }


{-| Specify whether a list should be a _two line_ list

Two line lists feature list items with a primary and a secondary text line.

-}
setTwoLine :: Boolean -> Config msg -> Config msg
setTwoLine twoLine (Config config_) =
    Config { config_ | twoLine = twoLine }


{-| Specify whether a list should wrap focus

A list that wraps focus focuses the first list item after pressing tab on the
last list item. By default, a list in that case passes focus to the next
focusable control.

-}
setWrapFocus :: Boolean -> Config msg -> Config msg
setWrapFocus wrapFocus (Config config_) =
    Config { config_ | wrapFocus = wrapFocus }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Array view function

The list view function takes its list items as two arguments. The first
argument represents the first list item, and the second argument reresents the
remaining list items. This way we guarantee lists to be non-empty.

-}
list :: Config msg -> ArrayItem msg -> Array (ArrayItem msg) -> Html msg
list ((Config { additionalAttributes }) as config_) firstArrayItem remainingArrayItems =
    let
        listItems =
            firstArrayItem :: remainingArrayItems
    in
    Html.node "mdc-list"
        (Array.filterMap identity
            [ rootCs
            , nonInteractiveCs config_
            , denseCs config_
            , avatarArrayCs config_
            , twoLineCs config_
            , wrapFocusProp config_
            , clickHandler listItems
            , selectedIndexProp listItems
            ]
            ++ additionalAttributes
        )
        (Array.map
            (\listItem_ ->
                case listItem_ of
                    ArrayItem.ArrayItem (ArrayItem.Config { node }) ->
                        node

                    ArrayItem.ArrayItemDivider node ->
                        node

                    ArrayItem.ArrayGroupSubheader node ->
                        node
            )
            listItems
        )


rootCs :: Maybe (Html.Attribute msg)
rootCs =
    Just (class "mdc-list")


nonInteractiveCs :: Config msg -> Maybe (Html.Attribute msg)
nonInteractiveCs (Config { nonInteractive }) =
    if nonInteractive then
        Just (class "mdc-list--non-interactive")

    else
        Nothing


denseCs :: Config msg -> Maybe (Html.Attribute msg)
denseCs (Config { dense }) =
    if dense then
        Just (class "mdc-list--dense")

    else
        Nothing


avatarArrayCs :: Config msg -> Maybe (Html.Attribute msg)
avatarArrayCs (Config { avatarArray }) =
    if avatarArray then
        Just (class "mdc-list--avatar-list")

    else
        Nothing


twoLineCs :: Config msg -> Maybe (Html.Attribute msg)
twoLineCs (Config { twoLine }) =
    if twoLine then
        Just (class "mdc-list--two-line")

    else
        Nothing


clickHandler :: Array (ArrayItem msg) -> Maybe (Html.Attribute msg)
clickHandler listItems =
    let
        getOnClick listItem_ =
            case listItem_ of
                ArrayItem.ArrayItem (ArrayItem.Config { onClick }) ->
                    Just onClick

                ArrayItem.ArrayItemDivider _ ->
                    Nothing

                ArrayItem.ArrayGroupSubheader _ ->
                    Nothing

        nthOnClick index =
            listItems
                |> Array.map getOnClick
                |> Array.filterMap identity
                |> Array.drop index
                |> Array.head
                |> Maybe.andThen identity

        mergedClickHandler =
            Decode.at [ "detail", "index" ] Decode.int
                |> Decode.andThen
                    (\index ->
                        case nthOnClick index of
                            Just msg_ ->
                                Decode.succeed msg_

                            Nothing ->
                                Decode.fail ""
                    )
    in
    Just (Html.Events.on "MDCArray:action" mergedClickHandler)


selectedIndexProp :: Array (ArrayItem msg) -> Maybe (Html.Attribute msg)
selectedIndexProp listItems =
    let
        selectedIndex =
            listItems
                |> Array.filter
                    (\listItem_ ->
                        case listItem_ of
                            ArrayItem.ArrayItem _ ->
                                True

                            ArrayItem.ArrayItemDivider _ ->
                                False

                            ArrayItem.ArrayGroupSubheader _ ->
                                False
                    )
                |> Array.indexedMap
                    (\index listItem_ ->
                        case listItem_ of
                            ArrayItem.ArrayItem (ArrayItem.Config { selection }) ->
                                if selection /= Nothing then
                                    Just index

                                else
                                    Nothing

                            ArrayItem.ArrayItemDivider _ ->
                                Nothing

                            ArrayItem.ArrayGroupSubheader _ ->
                                Nothing
                    )
                |> Array.filterMap identity
    in
    Just (Html.Attributes.property "selectedIndex" (Encode.list Encode.int selectedIndex))


{-| Array group view function
-}
group :: Array (IProp r i) -> Array (Html msg) -> Html msg
group additionalAttributes nodes =
    Html.div (listGroupCs :: additionalAttributes) nodes


listGroupCs :: Html.Attribute msg
listGroupCs =
    class "mdc-list-group"


{-| Array group subheader view function
-}
subheader :: Array (IProp r i) -> Array (Html msg) -> Html msg
subheader additionalAttributes nodes =
    Html.span (listGroupSubheaderCs :: additionalAttributes) nodes


listGroupSubheaderCs :: Html.Attribute msg
listGroupSubheaderCs =
    class "mdc-list-group__subheader"


wrapFocusProp :: Config msg -> Maybe (Html.Attribute msg)
wrapFocusProp (Config { wrapFocus }) =
    Just (Html.Attributes.property "wrapFocus" (Encode.bool wrapFocus))
