module HalogenMWC.ImageList where

import Protolude
import DOM.HTML.Indexed as I
import MaterialIconsFont.Classes
import Web.Event.Event

import Data.Array as Array
import Data.Maybe as Maybe
import Halogen
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.ImageList.Item (ImageListItem)
import HalogenMWC.ImageList.Item as ImageList.Item
import Material.Classes.ImageList
import Material.Classes.Ripple

type Config i =
  { masonry :: Boolean
  , withTextProtection :: Boolean
  , additionalAttributes :: Array (IProp I.HTMLdiv i)
  }

defaultConfig :: forall i . Config i
defaultConfig =
  { masonry: false
  , withTextProtection: false
  , additionalAttributes: []
  }

imageArray :: forall w i . Config i -> Array (ImageListItem w i) -> HH.HTML w i
imageArray config listItems =
  HH.element (ElemName "mdc-image-list")
    ( [ HP.classes $ Array.catMaybes
        [ Just mdc_image_list
        , if config.masonry then Just mdc_image_list____masonry else Nothing
        , if config.withTextProtection then Just mdc_image_list____with_text_protection else Nothing
        ]
      ]
      <> config.additionalAttributes
    )
    (map (listItemElt config) listItems)

listItemElt :: forall w i . Config i -> ImageListItem w i -> HH.HTML w i
listItemElt config (ImageList.Item.ImageListItem imageListItem) =
  let
    inner =
      [ ( if config.masonry
          then imageElt
          else imageAspectContainerElt
        ) config.masonry (ImageList.Item.ImageListItem imageListItem)
      , supportingElt (ImageList.Item.ImageListItem imageListItem)
      ]
  in
    HH.element (ElemName "mdc-image-list-item")
      ([ HP.class_ mdc_image_list__item ] <> imageListItem.additionalAttributes)
      ( imageListItem.href
          # map (\href_ -> [ HH.a [ HP.href href_ ] inner ])
          # Maybe.fromMaybe inner
      )

imageAspectContainerElt :: forall w i . Boolean -> ImageListItem w i -> HH.HTML w i
imageAspectContainerElt masonry (ImageList.Item.ImageListItem imageListItem) =
  HH.div
    [ HP.classes $ Array.catMaybes
        [ Just mdc_image_list__image_aspect_container
        , map (const mdc_ripple_surface) imageListItem.href
        ]
    ]
    [ imageElt masonry (ImageList.Item.ImageListItem imageListItem)
    ]

imageElt :: forall w i . Boolean -> ImageListItem w i -> HH.HTML w i
imageElt masonry (ImageList.Item.ImageListItem imageListItem) =
  let
    img =
      HH.img
        [ HP.class_ mdc_image_list__image
        , HP.src imageListItem.image
        ]
  in
    if masonry then
      if imageListItem.href /= Nothing then
        HH.div [ HP.class_ mdc_ripple_surface ] [ img ]
      else
        img
    else
      HH.div
        [ HP.class_ mdc_image_list__image
        , HP.attr (AttrName "style") ("background-image: url('" <> imageListItem.image <> "');")
        ]
        []

supportingElt :: forall w i . ImageListItem w i -> HH.HTML w i
supportingElt (ImageList.Item.ImageListItem imageListItem) =
  case imageListItem.label of
    Just string ->
      HH.div
        [ HP.class_ mdc_image_list__supporting ]
        [ HH.span [ HP.class_ mdc_image_list__label ] [ HH.text string ] ]
    Nothing -> HH.text ""
