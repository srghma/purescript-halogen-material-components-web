module HalogenMWC.Array.Item.Internal
    ( Config(..)
    , ArrayItem(..)
    , Selection(..)
    ) where

import Html (Html)


type Config r i
    = Config
        { disabled :: Boolean
        , selection :: Maybe Selection
        , href :: Maybe String
        , target :: Maybe String
        , additionalAttributes :: Array (IProp r i)
        , onClick :: Maybe msg
        , node :: Html msg
        }


data Selection
    = Selected
    | Activated


data ArrayItem msg
    = ArrayItem (Config msg)
    | ArrayItemDivider (Html msg)
    | ArrayGroupSubheader (Html msg)
