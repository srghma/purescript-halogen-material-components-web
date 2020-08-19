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
        , onClick :: Maybe r i
        , node :: Html r i
        }


data Selection
    = Selected
    | Activated


data ArrayItem r i
    = ArrayItem (Config r i)
    | ArrayItemDivider (Html r i)
    | ArrayGroupSubheader (Html r i)
