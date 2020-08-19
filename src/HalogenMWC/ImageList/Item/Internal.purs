module HalogenMWC.ImageList.Item.Internal (Config(..), ImageListItem(..))

import Html


data Config msg
    = Config
        { label :: Maybe String
        , href :: Maybe String
        , additionalAttributes :: List (Html.Attribute msg)
        , image :: String
        }


data ImageListItem msg
    = ImageListItem (Config msg)
