module HalogenMWC.Array.Item
    ( Config, config






    , ArrayItem, listItem
    , graphic
    , meta
    , text
    , Selection, selected
    , activated
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.Array.Item.Internal (Config(..), ArrayItem(..), Selection(..))



type Config r i =
    Material.Array.Item.Internal.Config r i



config :: Config r i
config =
    Config
        { disabled = False
        , selection = Nothing
        , href = Nothing
        , target = Nothing
        , additionalAttributes = []
        , onClick = Nothing
        , node = HH.text ""
        }








data Selection =
    Material.Array.Item.Internal.Selection



selected :: Selection
selected =
    Selected



activated :: Selection
activated =
    Activated



data ArrayItem r i =
    Material.Array.Item.Internal.ArrayItem r i

listItem :: Config r i -> Array (Html r i) -> ArrayItem r i
listItem (Config ({ additionalAttributes, href } as config_)) nodes =
    ArrayItem (Config { config_ | node = listItemView (Config config_) nodes })


listItemView :: Config r i -> Array (Html r i) -> Html r i
listItemView ((Config { additionalAttributes, href }) as config_) nodes =
    (\attributes ->
        if href /= Nothing then
            HH.node "mdc-list-item" [] [ HH.a attributes nodes ]

        else
            HH.node "mdc-list-item" attributes nodes
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
            <> additionalAttributes
        )


listItemCs :: Maybe (HH.Attribute r i)
listItemCs =
    Just (HP.class_ mdc_list_item)


disabledCs :: Config r i -> Maybe (HH.Attribute r i)
disabledCs (Config { disabled }) =
    if disabled then
        Just (HP.class_ mdc_list_item____disabled)

    else
        Nothing


selectedCs :: Config r i -> Maybe (HH.Attribute r i)
selectedCs (Config { selection }) =
    if selection == Just Selected then
        Just (HP.class_ mdc_list_item____selected)

    else
        Nothing


activatedCs :: Config r i -> Maybe (HH.Attribute r i)
activatedCs (Config { selection }) =
    if selection == Just Activated then
        Just (HP.class_ mdc_list_item____activated)

    else
        Nothing


ariaSelectedAttr :: Config r i -> Maybe (HH.Attribute r i)
ariaSelectedAttr (Config { selection }) =
    if selection /= Nothing then
        Just (HH.Attributes.attribute "aria-selected" "true")
    else
        Nothing


hrefAttr :: Config r i -> Maybe (HH.Attribute r i)
hrefAttr (Config { href }) =
    Maybe.map HH.Attributes.href href


targetAttr :: Config r i -> Maybe (HH.Attribute r i)
targetAttr (Config { href, target }) =
    if href /= Nothing then
        Maybe.map HH.Attributes.target target

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
    HH.div (HP.class_ mdc_list_item__text] <> additionalAttributes)
        [ primaryText [] primary
        , secondaryText [] secondary
        ]


primaryText :: Array (IProp r i) -> Array (Html r i) -> Html r i
primaryText additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__primary_text] <> additionalAttributes) nodes


secondaryText :: Array (IProp r i) -> Array (Html r i) -> Html r i
secondaryText additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__secondary_text] <> additionalAttributes) nodes



graphic :: Array (IProp r i) -> Array (Html r i) -> Html r i
graphic additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__graphic] <> additionalAttributes) nodes



meta :: Array (IProp r i) -> Array (Html r i) -> Html r i
meta additionalAttributes nodes =
    HH.div ([HP.class_ mdc_list_item__meta] <> additionalAttributes) nodes
