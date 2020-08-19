module HalogenMWC.List.Item.Internal
    ( Config(..)
    , ListItem(..)
    , Selection(..)
    ) where

import Html (Html)


data Config msg
    = Config
        { disabled :: Bool
        , selection :: Maybe Selection
        , href :: Maybe String
        , target :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        , onClick :: Maybe msg
        , node :: Html msg
        }


data Selection
    = Selected
    | Activated


data ListItem msg
    = ListItem (Config msg)
    | ListItemDivider (Html msg)
    | ListGroupSubheader (Html msg)
