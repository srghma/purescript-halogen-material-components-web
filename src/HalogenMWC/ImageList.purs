module HalogenMWC.ImageArray
    ( Config, config
    , setMasonry
    , setWithTextProtection
    , setAttributes
    , imageArray
    ) where

{-| An Image Array consists of several items, each containing an image and
optionally supporting a text label.

This modules concerns the container image list. If you are looking for
information about the image list items, refer to
[Material.ImageArray.Item](Material-ImageArray-Item).


# Table of Contents

  - [Resources](#resources)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Basic Usage](#basic-usage)
  - [Image Array](#image-list)
  - [Masonry Image Array](#masonry-image-list)
  - [Image Array with Text Label](#image-list-with-text-label)


# Resources

  - [Demo: Image Arrays](https://aforemny.github.io/material-components-web-elm/#image-list)
  - [Material Design Guidelines: Image list](https://material.io/go/design-image-list)
  - [MDC Web: Image Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-image-list)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-image-list#sass-mixins)


# Basic Usage

Note that you will have to set the width and margin of image list items
yourself, preferably through SASS or through inline CSS.

    
    import Material.ImageArray as ImageArray
    import Material.ImageArray.Item as ImageArrayItem

    main =
        ImageArray.imageArray ImageArray.config
            [ ImageArrayItem.imageArrayItem
                (ImageArrayItem.config
                    |> ImageArrayItem.setAttributes
                        [ style "width" "calc(100% / 5 - 4px)"
                        , style "margin" "2px"
                        ]
                )
                "images/photos/3x2/1.jpg"
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setMasonry
@docs setWithTextProtection
@docs setAttributes


# Image Array

@docs imageArray


# Masonry Image Array

The _masonry image list_ variant presents images vertically arranged into
several columns. In this layout, images may be any combination of aspect
ratios.

    ImageArray.imageArray
        (ImageArray.config |> ImageArray.setMasonry True)
        []


# Image Array with Label

Image's labels are by default positioned below the image. If you want image
labels to be positioned in a scrim overlaying each image, use the image list's
`setWithTextProtection` configuration option.

    ImageArray.imageArray
        (ImageArray.config
            |> ImageArray.setWithTextProtection True
        )
        [ ImageArrayItem.imageArrayItem
            (ImageArrayItem.config
                |> ImageArrayItem.setLabel (Just "Photo")
            )
            "images/photos/3x2/1.jpg"
        ]

-}

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import Material.ImageArray.Item (ImageArrayItem)
import Material.ImageArray.Item.Internal as ImageArrayItem


{-| Configuration of an image list
-}
type Config r i
    = Config
        { masonry :: Boolean
        , withTextProtection :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }


{-| Default configuration of an image list
-}
config :: Config r i
config =
    Config
        { masonry = False
        , withTextProtection = False
        , additionalAttributes = []
        }


{-| Specify whether an image list is a _masonry image list_

The masonry image list variant presents images vertically arranged into several
columns. In this layout, images may be any combination of aspect ratios.

-}
setMasonry :: Boolean -> Config r i -> Config r i
setMasonry masonry (Config config_) =
    Config { config_ | masonry = masonry }


{-| Specify whether an image list item's label should display in a scrim on top
of the image

By default, image list item's labels display below the image.

-}
setWithTextProtection :: Boolean -> Config r i -> Config r i
setWithTextProtection withTextProtection (Config config_) =
    Config { config_ | withTextProtection = withTextProtection }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Image list view function
-}
imageArray :: Config r i -> Array (ImageArrayItem r i) -> Html r i
imageArray ((Config { additionalAttributes }) as config_) listItems =
    Html.node "mdc-image-list"
        (Array.filterMap identity
            [ rootCs
            , masonryCs config_
            , withTextProtectionCs config_
            ]
            ++ additionalAttributes
        )
        (Array.map (listItemElt config_) listItems)


rootCs :: Maybe (Html.Attribute r i)
rootCs =
    Just (class "mdc-image-list")


masonryCs :: Config r i -> Maybe (Html.Attribute r i)
masonryCs (Config { masonry }) =
    if masonry then
        Just (class "mdc-image-list--masonry")

    else
        Nothing


withTextProtectionCs :: Config r i -> Maybe (Html.Attribute r i)
withTextProtectionCs (Config { withTextProtection }) =
    if withTextProtection then
        Just (class "mdc-image-list--with-text-protection")

    else
        Nothing


listItemElt :: Config r i -> ImageArrayItem r i -> Html r i
listItemElt ((Config { masonry }) as config_) ((ImageArrayItem.ImageArrayItem (ImageArrayItem.Config { href, additionalAttributes })) as listItem) =
    let
        inner =
            [ if masonry then
                imageElt masonry listItem

              else
                imageAspectContainerElt masonry listItem
            , supportingElt listItem
            ]
    in
    Html.node "mdc-image-list-item"
        (class "mdc-image-list__item" :: additionalAttributes)
        (href
            |> Maybe.map (\href_ -> [ Html.a [ Html.Attributes.href href_ ] inner ])
            |> Maybe.withDefault inner
        )


imageAspectContainerElt :: Boolean -> ImageArrayItem r i -> Html r i
imageAspectContainerElt masonry ((ImageArrayItem.ImageArrayItem (ImageArrayItem.Config { href })) as listItem) =
    Html.div
        (Array.filterMap identity
            [ Just (class "mdc-image-list__image-aspect-container")
            , Maybe.map (\_ -> class "mdc-ripple-surface") href
            ]
        )
        [ imageElt masonry listItem ]


imageElt :: Boolean -> ImageArrayItem r i -> Html r i
imageElt masonry (ImageArrayItem.ImageArrayItem (ImageArrayItem.Config { href, image })) =
    let
        img =
            Html.img
                [ class "mdc-image-list__image"
                , Html.Attributes.src image
                ]
                []
    in
    if masonry then
        if href /= Nothing then
            Html.div [ class "mdc-ripple-surface" ] [ img ]

        else
            img

    else
        Html.div
            [ class "mdc-image-list__image"
            , style "background-image" ("url('" ++ image ++ "')")
            ]
            []


supportingElt :: ImageArrayItem r i -> Html r i
supportingElt (ImageArrayItem.ImageArrayItem (ImageArrayItem.Config { label })) =
    case label of
        Just string ->
            Html.div
                [ class "mdc-image-list__supporting" ]
                [ Html.span [ class "mdc-image-list__label" ] [ text string ] ]

        Nothing ->
            text ""
