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



setInset :: Boolean -> Config r i -> Config r i
setInset inset (Config config_) =
    Config { config_ | inset = inset }



setPadded :: Boolean -> Config r i -> Config r i
setPadded padded (Config config_) =
    Config { config_ | padded = padded }



setAttributes :: Array (IProp r i) -> Config r i -> Config r i
setAttributes additionalAttributes (Config config_) =
    Config { config_ | additionalAttributes = additionalAttributes }



listItem :: Config r i -> ArrayItem r i
listItem ((Config { additionalAttributes }) as config_) =
    ArrayItem.ArrayItemDivider $
        Html.li
            (Array.filterMap identity
                [ listDividerCs
                , separatorRoleAttr
                , insetCs config_
                , paddedCs config_
                ]
                ++ additionalAttributes
            )
            []


listDividerCs :: Maybe (Html.Attribute r i)
listDividerCs =
    Just (HP.class_ mdc_list_divider)


separatorRoleAttr :: Maybe (Html.Attribute r i)
separatorRoleAttr =
    Just (Html.Attributes.attribute "role" "separator")


insetCs :: Config r i -> Maybe (Html.Attribute r i)
insetCs (Config { inset }) =
    if inset then
        Just (HP.class_ "mdc-list-divider--inset")

    else
        Nothing


paddedCs :: Config r i -> Maybe (Html.Attribute r i)
paddedCs (Config { padded }) =
    if padded then
        Just (HP.class_ "mdc-list-divider--padded")

    else
        Nothing



group :: Array (IProp r i) -> Html r i
group additionalAttributes =
    Html.hr (Array.filterMap identity [ listDividerCs ] ++ additionalAttributes) []
