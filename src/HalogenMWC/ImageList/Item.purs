module HalogenMWC.ImageArray.Item
    ( Config, config
    , setLabel
    , setHref
    , setAttributes
    , ImageArrayItem, imageArrayItem
    ) where

{-| An Image Array consists of several items, each containing an image and
optionally supporting a text label.

This modules concerns the image list item. If you are looking for information
about the image list contianer, refer to
[Material.ImageArray](Material-ImageArray).


# Table of Contents

  - [Resources](#resources)
  - [Configuration](#configuration)
      - [Configuration Options](#configuration-options)
  - [Basic Usage](#basic-usage)
  - [Image Array Item](#image-list-item)


# Resources

  - [Demo: Image Arrays](https://aforemny.github.io/material-components-web-elm/#image-list)
  - [Material Design Guidelines: Image list](https://material.io/go/design-image-list)
  - [MDC Web: Image Array](https://github.com/material-components/material-components-web/tree/master/packages/mdc-image-list)
  - [Sass Mixins (MDC Web)](https://github.com/material-components/material-components-web/tree/master/packages/mdc-image-list#sass-mixins)


# Basic Usage

Note that you will have to set the width and margin of image list items
yourself, preferably through SASS or through inline CSS.

    import Html.Attributes (style)
    import Material.ImageArray as ImageArray
    import Material.ImageArray.Item as ImageArrayItem

    main =
        ImageArray.imageArray ImageArray.config
            [ ImageArrayItem.imageArrayItem
                (ImageArray.itemConfig
                    |> ImageArray.setAttributes
                        [ style "width" "calc(100% / 5 - 4px)"
                        , style "margin" "2px"
                        ]
                )
                "images/photos/3x2/1.jpg"
            ]


# Configuration

@docs Config, config


## Configuration Options

@docs setLabel
@docs setHref
@docs setAttributes


# Image list Item

@docs ImageArrayItem, imageArrayItem

-}

import Html
import Material.ImageArray.Item.Internal (Config(..), ImageArrayItem(..))


{-| Configuration of an image list item
-}
type Config r i =
    Material.ImageArray.Item.Internal.Config msg


{-| Default configuration of an image list item
-}
config :: Config msg
config =
    Config
        { label = Nothing
        , href = Nothing
        , additionalAttributes = []
        , image = ""
        }


{-| Specify an image list item's label
-}
setLabel :: Maybe String -> Config msg -> Config msg
setLabel label (Config config_) =
    Config { config_ | label = label }


{-| Specify whether an image list item is supposed to be a _link image list item_

A link image list items behaves essentially like a HTML5 anchor element.

-}
setHref :: Maybe String -> Config msg -> Config msg
setHref href (Config config_) =
    Config { config_ | href = href }


{-| Specify additional attributes
-}
setAttributes :: Array (IProp r i) -> Config msg -> Config msg
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }


{-| Image list item type

Image list items can only be rendered within a [image list
container](Material-ImageArray)

-}
data ImageArrayItem msg =
    Material.ImageArray.Item.Internal.ImageArrayItem msg


{-| Image list item constructor
-}
imageArrayItem :: Config msg -> String -> ImageArrayItem msg
imageArrayItem (Config config_) image =
    ImageArrayItem (Config { config_ | image = image })
