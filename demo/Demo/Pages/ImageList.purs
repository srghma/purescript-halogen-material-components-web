module Demo.Pages.ImageList where

import Demo.HOC.CatalogPage (CatalogPage)
import Demo.Utils
import Halogen
import Material.Classes.Typography
import Protolude

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen as H
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.ImageList as ImageList
import HalogenMWC.ImageList.Item as ImageList.Item

config :: CatalogPage
config =
    { title: "Image List"
    , prelude: "Image lists display a collection of images in an organized grid."
    , resources:
        { materialDesignGuidelines: Just "https://material.io/go/design-image-list"
        , documentation: Just "https://package.elm-lang.org/packages/aforemny/material-components-web-elm/latest/Material-ImageList"
        , sourceCode: Just "https://github.com/material-components/material-components-web/tree/master/packages/mdc-image-list"
        }
    , hero: mkComponentStatic $ HH.div_
        [ ImageList.imageList
            (ImageList.defaultConfig
              { additionalAttributes = [ HP.style "width: 300px;" ]
              }
            )
            (Array.replicate 15 imageListHeroItem)
        ]
    , content: mkComponentStatic $ HH.div_
        [ HH.h3
            [ HP.class_ mdc_typography____subtitle1 ]
            [ HH.text "Standard Image Array with Text Protection" ]
        , standardImageList
        , HH.h3
            [ HP.class_ mdc_typography____subtitle1 ]
            [ HH.text "Masonry Image List" ]
        , masonryImageList
        ]
    }

standardImageList :: forall w i . HH.HTML w i
standardImageList =
    ImageList.imageList
        (ImageList.defaultConfig
            { withTextProtection = true
            , additionalAttributes = [ HP.style "max-width: 900px;" ]
            }
        )
        (map standardItem standardImages)

masonryImageList :: forall w i . HH.HTML w i
masonryImageList =
    ImageList.imageList
        (ImageList.defaultConfig
            { masonry = true
            , additionalAttributes = [ HP.style "max-width: 900px; column-count: 5; column-gap: 16px;" ]
            }
        )
        (map masonryItem masonryImages)

imageListHeroItem :: forall r w i . ImageList.Item.ImageListItem w i
imageListHeroItem =
    ImageList.Item.ImageListItem
        (ImageList.Item.defaultConfig
            { additionalAttributes =
                [ HP.style "width: calc(100% / 5 - 4.2px); margin: 2px;"
                ]
            , image = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAAXNSR0IArs4c6QAAAAlwSFlzAAAWJQAAFiUBSVIk8AAAABNJREFUCB1jZGBg+A/EDEwgAgQADigBA//q6GsAAAAASUVORK5CYII%3D"
            }
        )

standardItem :: forall r w i . String -> ImageList.Item.ImageListItem w i
standardItem image =
    -- TODO:
    -- ImageList.imageAspectContainer
    --   [ HP.style "padding-bottom: 66.66667%;" ]
    ImageList.Item.ImageListItem
        (ImageList.Item.defaultConfig
            { label = Just "Text label"
            , additionalAttributes =
                [ HP.style "width: calc(100% / 5 - 4.2px); margin: 2px;"
                ]
            , image = image
            }
        )

masonryItem :: forall r w i . String -> ImageList.Item.ImageListItem w i
masonryItem image =
    ImageList.Item.ImageListItem
        (ImageList.Item.defaultConfig
            { label = Just "Text label"
            , additionalAttributes = [ HP.style "margin-bottom: 16px;" ]
            , image = image
            }
        )

standardImages :: forall r w i . Array String
standardImages =
    [ "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/1.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/2.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/3.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/4.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/5.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/6.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/7.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/8.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/9.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/10.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/11.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/12.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/13.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/14.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/15.jpg"
    ]

masonryImages :: forall r w i . Array String
masonryImages =
    [ "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/16.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/1.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/1.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/2.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/3.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/2.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/4.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/3.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/5.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/4.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/6.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/5.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/2x3/7.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/6.jpg"
    , "https://aforemny.github.io/material-components-web-elm/images/photos/3x2/7.jpg"
    ]
