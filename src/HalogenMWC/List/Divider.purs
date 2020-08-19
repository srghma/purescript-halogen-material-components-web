module HalogenMWC.Array.Divider where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.Array.Item (ArrayItem)
import HalogenMWC.Array.Item as ArrayItem

type Config r i
    =
        { inset :: Boolean
        , padded :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }

defaultConfig :: Config r i
defaultConfig =
        { inset: False
        , padded: False
        , additionalAttributes: []
        }

listItem :: Config r i -> ArrayItem r i
listItem (config_@{ additionalAttributes }) =
    ArrayItem.ArrayItemDivider $
        HH.li
            (Array.filterMap identity
                [ listDividerCs
                , separatorRoleAttr
                , insetCs config_
                , paddedCs config_
                ]
                <> additionalAttributes
            )
            []

listDividerCs :: Maybe (IProp r i)
listDividerCs =
    Just (HP.class_ mdc_list_divider)

separatorRoleAttr :: Maybe (IProp r i)
separatorRoleAttr =
    Just (HH.Attributes.attribute "role" "separator")

in
insetCs { inset } =
    if inset then
        Just (HP.class_ mdc_list_divider____inset)

    else
        Nothing

paddedCs :: Config r i -> Maybe (IProp r i)
paddedCs { padded } =
    if padded then
        Just (HP.class_ mdc_list_divider____padded)

    else
        Nothing

group :: Array (IProp r i) -> Html r i
group additionalAttributes =
    HH.hr (Array.filterMap identity [ listDividerCs ] <> additionalAttributes) []
