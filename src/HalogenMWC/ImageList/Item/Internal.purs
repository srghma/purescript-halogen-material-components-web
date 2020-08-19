module Material.ImageList.Item.Internal exposing (Config(..), ImageListItem(..))

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
