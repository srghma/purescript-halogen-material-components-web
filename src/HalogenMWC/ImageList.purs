module HalogenMWC.ImageArray where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.ImageArray.Item (ImageArrayItem)
import HalogenMWC.ImageArray.Item as ImageArrayItem

type Config r i
    =
        { masonry :: Boolean
        , withTextProtection :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }

defaultConfig :: Config r i
defaultConfig =
        { masonry: False
        , withTextProtection: False
        , additionalAttributes: []
        }

imageArray :: Config r i -> Array (ImageArrayItem r i) -> Html r i
imageArray (config_@{ additionalAttributes }) listItems =
    HH.node "mdc-image-list"
        (Array.filterMap identity
            [ rootCs
            , masonryCs config_
            , withTextProtectionCs config_
            ]
            <> additionalAttributes
        )
        (Array.map (listItemElt config_) listItems)

rootCs :: Maybe (HH.Attribute r i)
rootCs =
    Just (HP.class_ mdc_image_list)

masonryCs :: Config r i -> Maybe (HH.Attribute r i)
masonryCs { masonry } =
    if masonry then
        Just (HP.class_ mdc_image_list____masonry)

    else
        Nothing

withTextProtectionCs :: Config r i -> Maybe (HH.Attribute r i)
withTextProtectionCs { withTextProtection } =
    if withTextProtection then
        Just (HP.class_ "mdc-image-list--with-text-protection")

    else
        Nothing

listItemElt :: Config r i -> ImageArrayItem r i -> Html r i
listItemElt (config_@{ masonry }) ((ImageArrayItem.ImageArrayItem { href, additionalAttributes }) as listItem) =
    let
        inner =
            [ if masonry then
                imageElt masonry listItem

              else
                imageAspectContainerElt masonry listItem
            , supportingElt listItem
            ]
    in
    HH.node "mdc-image-list-item"
        ([HP.class_ mdc_image_list__item] <> additionalAttributes)
        (href
            # map (\href_ -> [ HH.a [ HH.Attributes.href href_ ] inner ])
            # Maybe.withDefault inner
        )

imageAspectContainerElt :: Boolean -> ImageArrayItem r i -> Html r i
imageAspectContainerElt masonry ((ImageArrayItem.ImageArrayItem { href }) as listItem) =
    HH.div
        (Array.filterMap identity
            [ Just (HP.class_ mdc_image_list__image_aspect_container)
            , map (\_ -> HP.class_ mdc_ripple_surface) href
            ]
        )
        [ imageElt masonry listItem ]

imageElt :: Boolean -> ImageArrayItem r i -> Html r i
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

supportingElt :: ImageArrayItem r i -> Html r i
supportingElt (ImageArrayItem.ImageArrayItem { label }) =
    case label of
        Just string ->
            HH.div
                [ HP.class_ mdc_image_list__supporting ]
                [ HH.span [ HP.class_ mdc_image_list__label ] [ text string ] ]

        Nothing ->
            text ""
