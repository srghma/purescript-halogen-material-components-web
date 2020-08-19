module HalogenMWC.ImageList where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML IProp
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA
import HalogenMWC.ImageList.Item ImageArrayItem
import HalogenMWC.ImageList.Item as ImageArrayItem

type Config r i
  = { masonry :: Boolean
    , withTextProtection :: Boolean
    , additionalAttributes :: Array (IProp r i)
    }

defaultConfig :: Config r i
defaultConfig =
  { masonry: false
  , withTextProtection: false
  , additionalAttributes: []
  }

imageArray :: Config r i -> Array (ImageArrayItem r i) -> HH.HTML w i
imageArray config_ listItems =
  HH.element "mdc-image-list"
    ( Array.filterMap identity
        [ HP.class_ mdc_image_list
        , masonryCs config_
        , withTextProtectionCs config_
        ]
        <> config_.additionalAttributes
    )
    (map (listItemElt config_) listItems)

masonryCs :: Config r i -> Maybe (IProp r i)
masonryCs { masonry } =
  if masonry then
    Just (HP.class_ mdc_image_list____masonry)
  else
    Nothing

withTextProtectionCs :: Config r i -> Maybe (IProp r i)
withTextProtectionCs { withTextProtection } =
  if withTextProtection then
    Just (HP.class_ mdc_image_list____with_text_protection)
  else
    Nothing

listItemElt :: Config r i -> ImageArrayItem r i -> HH.HTML w i
listItemElt (config_@{ masonry }) (listItem@(ImageArrayItem.ImageArrayItem { href, additionalAttributes })) =
  let
    inner =
      [ if masonry then
          imageElt masonry listItem
        else
          imageAspectContainerElt masonry listItem
      , supportingElt listItem
      ]
  in
    HH.element "mdc-image-list-item"
      ([ HP.class_ mdc_image_list__item ] <> additionalAttributes)
      ( href
          # map (\href_ -> [ HH.a [ HH.Attributes.href href_ ] inner ])
          # Maybe.withDefault inner
      )

imageAspectContainerElt :: Boolean -> ImageArrayItem r i -> HH.HTML w i
imageAspectContainerElt masonry (listItem@(ImageArrayItem.ImageArrayItem { href })) =
  HH.div
    ( Array.filterMap identity
        [ Just (HP.class_ mdc_image_list__image_aspect_container)
        , map (\_ -> HP.class_ mdc_ripple_surface) href
        ]
    )
    [ imageElt masonry listItem ]

imageElt :: Boolean -> ImageArrayItem r i -> HH.HTML w i
imageElt masonry (ImageArrayItem.ImageArrayItem { href, image }) =
  let
    img =
      HH.img
        [ HP.class_ mdc_image_list__image
        , HH.Attributes.src image
        ]
        []
  in
    if masonry then
      if href /= Nothing then
        HH.div [ HP.class_ mdc_ripple_surface ] [ img ]
      else
        img
    else
      HH.div
        [ HP.class_ mdc_image_list__image
        , style "background-image" ("url('" <> image <> "')")
        ]
        []

supportingElt :: ImageArrayItem r i -> HH.HTML w i
supportingElt (ImageArrayItem.ImageArrayItem { label }) = case label of
  Just string ->
    HH.div
      [ HP.class_ mdc_image_list__supporting ]
      [ HH.span [ HP.class_ mdc_image_list__label ] [ text string ] ]
  Nothing -> text ""
