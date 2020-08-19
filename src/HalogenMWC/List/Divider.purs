module HalogenMWC.Array.Divider
    ( Config, config



    , listItem
    , group
    ) where

import Protolude
import Halogen (AttrName(..))
import Halogen.HTML (IProp)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as Halogen.HTML.Properties.ARIA

import HalogenMWC.Array.Item (ArrayItem)
import HalogenMWC.Array.Item.Internal as ArrayItem



type Config r i
    =
        { inset :: Boolean
        , padded :: Boolean
        , additionalAttributes :: Array (IProp r i)
        }



config :: Config r i
config =
    Config
        { inset = False
        , padded = False
        , additionalAttributes = []
        }


















listItem :: Config r i -> ArrayItem r i
listItem ((Config { additionalAttributes }) as config_) =
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


listDividerCs :: Maybe (HH.Attribute r i)
listDividerCs =
    Just (HP.class_ mdc_list_divider)


separatorRoleAttr :: Maybe (HH.Attribute r i)
separatorRoleAttr =
    Just (HH.Attributes.attribute "role" "separator")


in
insetCs (Config { inset }) =
    if inset then
        Just (HP.class_ mdc_list_divider____inset)

    else
        Nothing


paddedCs :: Config r i -> Maybe (HH.Attribute r i)
paddedCs (Config { padded }) =
    if padded then
        Just (HP.class_ mdc_list_divider____padded)

    else
        Nothing



group :: Array (IProp r i) -> Html r i
group additionalAttributes =
    HH.hr (Array.filterMap identity [ listDividerCs ] <> additionalAttributes) []
