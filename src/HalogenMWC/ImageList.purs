module HalogenMWC.ImageArray
    ( Config, config



    , imageArray
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.ImageArray.Item (ImageArrayItem)
import HalogenMWC.ImageArray.Item.Internal as ImageArrayItem



type Config r i
    =
        { masonry :: Boolean
        , withTextProtection :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { masonry = False
        , withTextProtection = False
        , additionalAttributes = []
        }



setMasonry :: Boolean -> Config r i -> Config r i
setMasonry masonry (Config config_) =
    Config { config_ | masonry = masonry }



setWithTextProtection :: Boolean -> Config r i -> Config r i
setWithTextProtection withTextProtection (Config config_) =
    Config { config_ | withTextProtection = withTextProtection }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



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
            # Maybe.map (\href_ -> [ Html.a [ Html.Attributes.href href_ ] inner ])
            # Maybe.withDefault inner
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
